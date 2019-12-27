open Jest;
open Graphql_Language;

describe("Parse and print a graphql schema", () => {
  open Expect;

  let schema = {|
    type User @example {
      id: ID! @example
      firstName: String!
      lastName: String!
      nicknames: [String!]
      createdAt: DateTime!
      updatedAt: DateTime!
    }
  |};

  test("should parse schema correctly", () => {
    let maybeDocument = Parser.parse(schema);

    let out = maybeDocument->Belt.Result.getExn->Printer.print;
    Js.log(out);
    
    expect(Belt.Result.isOk(maybeDocument)) |> toBe(true);

    // expect(out) |> toBe(schema); TODO Some whitespace formatting issues
  });
});

describe("Parse and print a graphql query", () => {
  open Expect;

  let query = {|
    query My1stQuery($id: Int, $another: [String!]!) { field {
                hello: name
                stringField(name:"\"test\"",nameagain:"汉字")
        # Commentsssss
                age(default: 4.5, a: [4, 5, 5])
        ... on Foo {
                bar(h:{hello:5,nested:{world:1}})
                      ... on BarType {
                baz    @skip(if: $another) @skip(if: $another)
                ...bazFields
        }
        }
      }
    }

    fragment bazFields on FragmentModel {
      foo
      baz {
                g @skip(if: $another)
        ... on Bar {
          # dfffdfdfd
          b @skip(if: $another) @skip(if: {a: [1, 3, 4]})
                  c
            a
        }
      }
    }
 |};

  let document = Parser.parse(query);
  let out = document->Belt.Result.getExn->Printer.print;

  test("Should prettify the query correctly", () => {
    let pretty = {|query My1stQuery($id: Int, $another: [String!]!) {
  field {
    hello: name
    stringField(name: "\"test\"", nameagain: "汉字")
    age(default: 4.5, a: [4, 5, 5])
    ... on Foo {
      bar(h: {hello: 5, nested: {world: 1}})
      ... on BarType {
        baz @skip(if: $another) @skip(if: $another)
        ...bazFields
      }
    }
  }
}

fragment bazFields on FragmentModel {
  foo
  baz {
    g @skip(if: $another)
    ... on Bar {
      b @skip(if: $another) @skip(if: {a: [1, 3, 4]})
      c
      a
    }
  }
}|};

    expect(out) |> toBe(pretty);
  });
});