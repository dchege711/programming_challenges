---
date: 2024-06-16
domains:
- codeql.github.com
- github.com
- owasp.org
- www.mongodb.com
- www.npmjs.com
local_url: http://localhost:1313/computer-science/programming-challenges/flashcards-app/database-layer/
title: Database Layer
---

Currently using MongoDB's free tier, which has shared RAM, and up to 5GB
of storage {{% cite mongoDBPricing %}}. So far, the overall DB usage has
been less than 5MB.

## Query Injection

### Current State of Affairs

Currently have this protection implemented back in 2018:

```ts
/**
 * @description Prevent a NoSQL Injection in the search parameters. This is
 * achieved by deleting all query values that begin with `$`.
 */
export function sanitizeQuery(query: any) {
  const keys = Object.keys(query);
  for (let i = 0; i < keys.length; i++) {
    if (/^\$/.test(query[keys[i]])) { delete query[keys[i]]; }
  }
  return query;
}
```

{{% cite commit186da7c %}}

We can do better than roll out our own NoSQL injection protection; this
is a common enough problem that libraries should have protections for.

### MongoDB Injection Attacks

For MongoDB in a JS environment, one can set
`security.javascriptEnabled` to `false` in the `mongod` configuration to
disallow JS execution on the server. Otherwise, an attacker can run
arbitrary JS in these MongoDB operations: `$where`, `mapReduce`,
`$accumulator`, and `$function`. {{% cite MongoDBQueryInjection %}}

CodeQL did flag code like `User.findOne({email: payload.email})` as
`js/sql-injection` violation, and suggested `User.findOne({email: { $eq:
payload.email }})` instead. {{% cite commit0d57acc %}} The use of `eq`
ensures that the input is interpreted as a literal value and not a query
object. Alternatively, checking that `typeof payload.email === "string"`
also provides protection against NoSQL injection. {{% cite
codeQLJSSQLInjection %}}

{{% comment %}}

Reading {{% cite MongoDBQueryInjection %}}, I don't think I'd have
gotten to {{% cite commit0d57acc %}} without the hint from {{% cite
codeQLJSSQLInjection %}}. Does Mongoose's `Model.findOne` internally use
`$where` and that's why {{% cite codeQLJSSQLInjection %}} flags it?

{{% /comment %}}

{{% cite OWASPNoSQLInjection %}} has concrete examples of NoSQL
injection attacks:

```ts
db.myCollection.find({
  active: true,
  $where: function() { return obj.credits - obj.debits < $userInput; }
});

// A string containing any of these unsanitized characters would cause a
// database error.
const maliciousInput = `' " \ ; { }`;

// If inserted into `$userInput`, the MongoDB instance would execute at
// 100% CPU usage for 10 seconds.
const injection = `0;var date=new Date(); do{curDate = new Date();}while(curDate-date<10000)`;
```

{{% comment %}}

{{% cite OWASPNoSQLInjection %}} also brings up an alternate injection
attack in a language like PHP, where PHP would try to substitute
`$where` with the value of the variable `$where`. However, this doesn't
apply to the current project.

{{% /comment %}}

A sample NoSQL injection is navigating to
`/trpc/fetchPublicCard?batch=1&input=%7B%220%22%3A%7B%22cardID%22%3A%7B%22%24ne%22%3A%22000000000000000000000000%22%7D%7D%7D`.
The user should not be able to execute a query like `{cardID: {$ne:
"000000000000000000000000" }` and fetch a card. The tRPC endpoint should
be able to strip out the non-literal card ID.

What about vanilla Express endpoints? CodeQL highlighted the injection
attack surfaces for endpoints like `/login`, `/account`,
`/reset-password`, and `/send-validation-email` and we fixed them. {{%
cite commit0f5b088 %}} Seems like CodeQL's queries are good at finding
vulnerabilities in Express, but not in tRPC.

### NoSQL Injection Prevention Libraries

{{% cite mongo-sanitize %}} is similar to {{% cite commit186da7c %}} in
that it strips out keys that start with `$`, but goes one step further
by stripping out such keys recursively.

{{% cite express-mongo-sanitize %}} sanitizes keys that begin with
either `$` or `.`. It can also be used as middleware allowing it to
sanitize fields such as `req.body`, `req.params`, `req.headers`, and
`req.query`. It can also be used in a non-middleware context, allowing
it to be a drop-in replacement for {{% cite commit186da7c %}}.

{{% comment %}}

`db.inventory.find({"price.usd": { $gt: 40 }})` would match the document

```json
{
   "item" : "sweatshirt",
   "price": {
      "usd": 45.99
   },
   "quantity": 20
}
```

{{% cite periodsMongoDB %}}. Is that what {{% cite
express-mongo-sanitize %}} means by "`.` could change the context of a
database operation"?

{{% /comment %}}

Doing `app.use(mongoSanitize())` as advised by {{% cite
express-mongo-sanitize %}} doesn't prevent injection attacks via `tRPC`.

## References

1. {{< citation
  id="mongoDBPricing"
  title="Pricing | MongoDB"
  url="https://www.mongodb.com/pricing"
  accessed="2024-06-16" >}}

1. {{< citation
  id="commit186da7c"
  date="2018-11-16"
  title="Sanitize card content and queries. Store card HTML server-side. · dchege711/study_buddy@186da7c"
  url="https://github.com/dchege711/study_buddy/blob/7a77f7fb698f943a2f2ceb9ce572b3255be517a7/src/models/SanitizationAndValidation.ts#L108"
  url_2="https://github.com/dchege711/study_buddy/commit/186da7c7dab999ff582c4175f8e6388533abb6e1"
  accessed="2024-06-16" >}}

1. {{< citation
  id="MongoDBQueryInjection"
  title="FAQ: MongoDB Fundamentals - MongoDB Manual v7.0"
  url="https://www.mongodb.com/docs/manual/faq/fundamentals/#how-does-mongodb-address-sql-or-query-injection-"
  accessed="2024-06-16" >}}

1. {{< citation
  id="commit0d57acc"
  title="[DB] Fix js/sql-injection violations · dchege711/study_buddy@0d57acc"
  url="https://github.com/dchege711/study_buddy/commit/0d57acc4801f599d4e3460a147546e49234a5905"
  date="2024-06-08"
  accessed="2024-06-16" >}}

1. {{< citation
  id="codeQLJSSQLInjection"
  title="Database query built from user-controlled sources — CodeQL query help documentation"
  url="https://codeql.github.com/codeql-query-help/javascript/js-sql-injection/"
  accessed="2024-06-16" >}}

1. {{< citation
  id="OWASPNoSQLInjection"
  title="WSTG - Latest | OWASP Foundation"
  url="https://owasp.org/www-project-web-security-testing-guide/latest/4-Web_Application_Security_Testing/07-Input_Validation_Testing/05.6-Testing_for_NoSQL_Injection"
  accessed="2024-06-16" >}}

1. {{< citation
  id="mongo-sanitize"
  date="2020-03-02"
  title="mongo-sanitize - npm"
  url="https://www.npmjs.com/package/mongo-sanitize?activeTab=readme"
  accessed="2024-06-16" >}}

1. {{< citation
  id="express-mongo-sanitize"
  date="2022-01-14"
  title="express-mongo-sanitize - npm"
  url="https://www.npmjs.com/package/express-mongo-sanitize"
  accessed="2024-06-16" >}}

1. {{< citation
  id="periodsMongoDB"
  title="Field Names with Periods - MongoDB Manual v7.0"
  url="https://www.mongodb.com/docs/manual/core/dot-dollar-considerations/periods/"
  accessed="2024-06-16" >}}

1. {{< citation
  id="commit0f5b088"
  title="[DB] Fix js/sql-injection violations · dchege711/study_buddy@0f5b088"
  url="https://github.com/dchege711/study_buddy/commit/0f5b0882011c8f3721252ffa101e4ca7fa196f78"
  date="2024-06-09"
  accessed="2024-06-24" >}}
