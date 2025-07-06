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

## How to Serialize and Deserialize .NET Objects

To write JSON to a string, call `JsonSerializer.Serialize`. To read from a JSON
string, call `JsonSerializer.Deserialize<T>`. For example:

```cs
Foo foo = new(1, "two");

var jsonStr = JsonSerializer.Serialize(foo);
Console.WriteLine(jsonStr); // {"s":"two","n":1}

var roundTripSuccess = JsonSerializer.Deserialize<Foo>(jsonStr) == foo;
Console.WriteLine(roundTripSuccess); // True 

abstract record Base(int n);
record Foo(int n, string s): Base(n);
```

`JsonSerializer.Serialize<Base>(foo)` only serializes properties in `Base`,
i.e., `{"n":1}`. When deserializing, any properties not represented in your `T`
are ignored by default. {{% cite HowToSerialize %}} {{% cite HowToDeserialize
%}}

{{% comment %}}

Using `JsonSerializer.Serialize<T>(foo)` where `Foo` cannot be converted to a
`T` leads to a compiler error.

{{% /comment %}}

One can also serialize to and deserialize from a file using
`JsonSerializer.SerializeAsync` and `JsonSerializer.DeserializeAsync<T>`,
respectively, e.g.,

```cs
string fileName = "my-data.json";

await using FileStream outputStream = File.Create(fileName);
await JsonSerializer.SerializeAsync(outputStream, foo);

await using FileStream inputStream = File.OpenRead(fileName);
Foo? foo = await JsonSerializer.DeserializeAsync<Foo>(inputStream);
```

{{% open-comment %}}

There exist synchronous APIs as well, e.g., `File.WriteAllText` and
`File.ReadAllText`. When would one choose the synchronous versions over the
asynchronous versions? I've been under the impression that I/O should be done
async whenever possible.

{{% /open-comment %}}

{{% cite HowToSerialize %}} {{% cite HowToDeserialize %}}

If you don't have a `T` to deserialize into, you can use the `Utf8JsonReader`
directly, or deserialize into a `JsonNode` DOM, which lets you navigate to any
subsection of a JSON payload. {{% cite HowToDeserialize %}}

Types supported out-of-the-box include: .NET primitives that map to JavaScript
primitives; user-defined plain old CLR objects; `T[]` and `T[][]`, collections
and dictionaries from `System.Collections`. {{% cite HowToSerialize %}}

Because bytes (as UTF-8) don't need to be converted to strings (UTF-16), it is
5-10% faster to use `JsonSerializer.SerializeToUtf8Bytes` instead of
`JsonSerializer.Serialize` {{% cite HowToSerialize %}}. There is a
`JsonSerializer.Deserialize<T>` overload that takes in a `Utf8JsonReader` or a
`ReadOnlySpan<byte>` to deserialize the bytes. {{% cite HowToDeserialize %}}

`JsonSerializer.Serialize` accepts a `JsonSerializerOptions` to configure the
output, e.g.,

```cs
var options = new JsonSerializerOptions { WriteIndented = true };
string jsonString = JsonSerializer.Serialize(foo, options);
```

{{% cite HowToSerialize %}} {{% cite JsonSerializerOptions %}}

## How to Deserialize with Required Properties

If there are any `required` properties of `T` missing from the JSON payload,
then deserialization throws a `JsonException` at runtime. {{% cite
DeserializeWithRequiredProps %}}

```cs
public class Foo
{
    // Option 1: `s` is required in all contexts, even outside of serialization.
    public required string s { get; set; }

    // Option 2: `n` is required only in a serialization context.
    [JsonRequired]
    public int n {get; set; }
}
```

From a deserialization perspective, the `required` keyword is equivalent to the
`JsonRequired` attribute. The latter is useful when using source generation
because at compile time, the `required` constraint can't be satisfied. {{% cite
DeserializeWithRequiredProps %}}

The `required` constraint is checked at runtime. It's possible to control this
through the `TypeInfoResolver` passed to the `JsonSerializerOptions`, e.g., by
setting `JsonPropertyInfo.IsRequired`. {{% cite DeserializeWithRequiredProps %}}

With `JsonSerializerOptions.RespectRequiredConstructorParameters` set,
non-optional constructor parameters, e.g., `Name` in `record Person(string Name,
int? Age = null)`, are treated as required. {{% cite
DeserializeWithRequiredProps %}}

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

## How to Exclude Properties

By default, all public properties are serialized. The `JsonIgnore` attribute
allows us to ignore individual properties, e.g.,

```cs
public class Foo
{
    [JsonIgnore(Condition = JsonIgnoreCondition.Always)]
    public string s1 { get; set; } = "Paukwa"; // Will not be serialized.
    public string s2 { get; set; } = "Pakawa";
}
```

`JsonIgnoreCondition` values include `Always` (default), `Never`,
`WhenWritingDefault`, and `WhenWritingNull`. {{% cite IgnoreProperties %}}

Read-only properties are ones with a public getter but a non-public setter,
e.g., `public string s { get; private set; } = "Top Secret"`.
`JsonSerializerOptions`'s `IgnoreReadOnlyProperties` can be set to `true` to
override the default behavior where such properties get serialized. {{% cite
IgnoreProperties %}}

`JsonSerializerOptions`'s `DefaultIgnoreCondition` can be set to ignore
properties based on a criteria, e.g., `WhenWritingNull` to drop `null`s,
`WhenWritingDefault` to drop `default`s and `null`s. {{% cite IgnoreProperties
%}}

## How to Include Fields

{{% comment %}}

A `field` defines a storage location.

```cs
public class Person
{
  public string? FirstName;
}
```

... while a `property` is an outward-facing declaration:

```cs
public class Person
{
  // The compiler generates a hidden backing field, and implements the body of
  // the get and set accessors.
  public string FirstName { get; set; } = string.Empty;
}
```

Think of properties as smart fields. You can provide validation, lazy
evaluation, different accessibility, etc. {{% cite Properties %}}

{{% /comment %}}

By default, fields are not serialized. Set `JsonSerializerOptions.IncludeFields`
or use the `[JsonInclude]` attribute on them to include them. Set
`JsonSerializerOptions.IgnoreReadOnlyFields` to ignore fields marked with
`readonly`. {{% cite IncludeFields %}}

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

1. {{< citation
  id="IgnoreProperties"
  title="How to ignore properties with System.Text.Json - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/serialization/system-text-json/ignore-properties"
  accessed="2025-07-05" >}}

1. {{< citation
  id="Properties"
  title="Properties - C# | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/properties"
  accessed="2025-07-05" >}}

1. {{< citation
  id="IncludeFields"
  title="Include fields in serialization - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/serialization/system-text-json/fields"
  accessed="2025-07-05" >}}

1. {{< citation
  id="HowToDeserialize"
  title="How to deserialize JSON in C# - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/serialization/system-text-json/deserialization"
  accessed="2025-07-06" >}}

1. {{< citation
  id="DeserializeWithRequiredProps"
  title="Require properties for deserialization - .NET | Microsoft Learn"
  url="https://learn.microsoft.com/en-us/dotnet/standard/serialization/system-text-json/required-properties"
  accessed="2025-07-06" >}}
