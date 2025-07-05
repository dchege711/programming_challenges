---
date: 2025-07-05
domains:
- learn.microsoft.com
local_url: http://localhost:1313/computer-science/programming-challenges/language-concepts/data-wrangling/json-serialization/json-serialization-in-dotnet/
title: JSON Serialization in .NET
---

Serialization converts the state of an object (the value of its properties) into
a form that can be stored/transmitted. The serialized form doesn't include any
information about an object's associated methods. {{% cite
JsonSerializationOverview %}}

`System.Text.Json` emphasizes high performance and low memory allocation over an
extensive feature set. It has built-in UTF-8 support because UTF-8 is the most
prevalent encoding for data on the web and files on disk. {{% cite
JsonSerializationOverview %}}

## How to Serialize .NET Objects as JSON

To write JSON to a string, call `JsonSerializer.Serialize`, e.g.,

```cs
Foo foo = new(1, "two");
Console.WriteLine(JsonSerializer.Serialize(foo)); // {"s":"two","n":1}

abstract record Base(int n);
record Foo(int n, string s): Base(n);
```

`JsonSerializer.Serialize<Base>(foo)` onlys serialize properties in `Base`,
i.e., `{"n":1}`. {{% cite HowToSerialize %}}

{{% comment %}}

Using `JsonSerializer.Serialize<T>(foo)` where `Foo` cannot be converted to a
`T` leads to a compiler error.

{{% /comment %}}

One can also serialize to a file, e.g.,

```cs
await using FileStream outputStream = File.Create("my-data.json");
await JsonSerializer.SerializeAsync(outputStream, foo);
```

{{% cite HowToSerialize %}}

Types supported out-of-the-box include: .NET primitives that map to JavaScript
primitives; user-defined plain old CLR objects; `T[]` and `T[][]`, collections
and dictionaries from `System.Collections`. {{% cite HowToSerialize %}}

Because bytes (as UTF-8) don't need to be converted to strings (UTF-16), it is
5-10% faster to use `JsonSerializer.SerializeToUtf8Bytes` instead of
`JsonSerializer.Serialize`. {{% cite HowToSerialize %}}

`JsonSerializer.Serialize` accepts a `JsonSerializerOptions` to configure the
output, e.g.,

```cs
var options = new JsonSerializerOptions { WriteIndented = true };
string jsonString = JsonSerializer.Serialize(foo, options);
```

{{% cite HowToSerialize %}} {{% cite JsonSerializerOptions %}}

## How to Customize Property Names and Values

By default, properties are serialized with the same name and in the order in
which they are defined. `JsonNameProperty` and `JsonPropertyOrder` can modify
this, e.g.,

```cs
Foo foo = new Foo {};
// Output: {"nTwo":0,"n_One":0,"tatu":0}
Console.WriteLine(JsonSerializer.Serialize(foo));

public class Foo
{
    public int n_One { get; set; }

    [JsonPropertyOrder(-3)]
    public int nTwo { get; set; }

    [JsonPropertyName("tatu")]
    public int n_three { get; set; }
}
```

Without the attributes, the serialization would have been
`{"n_One":0,"nTwo":0,"n_three":0}`. {{% cite CustomizePropertyNamesAndValues %}}

`JsonSerializerOptions` has a `PropertyNamingPolicy` that takes in a policy for
generating names when serializing. The `JsonPropertyName` overrides this though:

```cs
Foo foo = new Foo {};

var options = new JsonSerializerOptions
{
    PropertyNamingPolicy = new ContosoNamingPolicy()
};
// Output: {"contoso_N_ONE":0,"contoso_NTWO":0,"tatu":0}
Console.WriteLine(JsonSerializer.Serialize(foo, options));

public class Foo
{
    public int n_One { get; set; }

    public int nTwo { get; set; }

    [JsonPropertyName("tatu")]
    public int n_three { get; set; }
}

public class ContosoNamingPolicy : JsonNamingPolicy
{
    public override string ConvertName(string name) =>
        $"contoso_{name.ToUpper()}";
}
```

There are several in-built `JsonNamingPolicy.*` classes, e.g., `CamelCase`
(`fooBar`), `KebabCaseLower` (`foo-bar`), `KebabCaseUpper` (`FOO-BAR`),
`SnakeCaseLower` (`foo_bar`) and `SnakeCaseUpper` (`FOO_BAR`). {{% cite
CustomizePropertyNamesAndValues %}}

If serializing a `Dictionary<string, TValue>`, then one can supply a
`JsonNamingPolicy` to `JsonSerializerOptions`'s `DictionaryKeyPolicy` to specify
how the keys will be serialized. However, unlike `PropertyNamingPolicy`,
`JsonNamingPolicy` only applies during serialization and not during
deserialization. {{% cite CustomizePropertyNamesAndValues %}}

{{% open-comment %}}

If `JsonNamingPolicy` does not support round-tripping data, then what use is it?
Seems like a risky API...

{{% /open-comment %}}

By default, enums are serialized as numbers. One can specify attributes on enums
to serialize them differently, e.g.,

```cs
[JsonConverter(typeof(JsonStringEnumConverter))]
public enum Foo
{
  Bar, // Output: Bar
  [JsonStringEnumMemberName("AlternateBazName")]
  Baz, // Output: AlternateBazName
  Qux  // Output: Qux
}
```

{{% cite CustomizePropertyNamesAndValues %}}

One can also apply converters to the `JsonSerializerOptions`, e.g.,

```cs
var options = new JsonSerializerOptions
{
    Converters = { new JsonStringEnumConverter(JsonNamingPolicy.CamelCase) }
};
Console.WriteLine(JsonSerializer.Serialize(Foo.Baz, options)); // Output: baz

public enum Foo { Bar, Baz, Qux }
```

{{% comment %}}

Of the various modifications to `JsonSerializerOptions`, serializing enums as
strings is my favorite. Helps a ton with readability when debugging data flows
because I don't need to maintain a map of what enumerator corresponds with a
given number. It also makes it easy to re-order enums because their underlying
numerical values are inconsequential.

{{% /comment %}}

## Reflection vs. Source Generation

By default, `System.Text.Json` gathers the metadata needed to access properties
of objects for serialization at run time using [reflection]({{< ref
"/computer-science/programming-challenges/language-concepts/meta-programming"
>}}). {{% cite JsonSerializationOverview %}}

Alternatively, `System.Text.Json` can use source generation to improve
performance, reduce private memory, and facilitate assembly trimming, which
reduces app size. {{% cite JsonSerializationOverview %}}

## References

1. {{< citation
  id="JsonSerializationOverview"
  title="Serialize and deserialize JSON using C# - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/serialization/system-text-json/overview"
  accessed="2025-07-05" >}}

1. {{< citation
  id="HowToSerialize"
  title="How to serialize JSON in C# - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/serialization/system-text-json/how-to"
  accessed="2025-07-05" >}}

1. {{< citation
  id="JsonSerializerOptions"
  title="JsonSerializerOptions Class (System.Text.Json) | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/api/system.text.json.jsonserializeroptions?view=net-9.0"
  accessed="2025-07-05" >}}

1. {{< citation
  id="CustomizePropertyNamesAndValues"
  title="How to customize property names and values with System.Text.Json - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/serialization/system-text-json/customize-properties"
  accessed="2025-07-05" >}}
