open Jest;
open Graphql_Language;

describe("Parse and print a graphql schema", () => {
  open Expect;

  let schema = {|
    schema @bingo {
      query: Query
      mutation: Mutation
      subscription: Subscription
    }

    type Query {
      foo(bar: String! = "baz"): String
      boop(baz: Float): Float
      user: User
    }

    directive @simple on OBJECT
    directive @example(
      sweet: Boolean! = true
    ) repeatable on OBJECT | FIELD_DEFINITION

    scalar DateTime
    scalar _Any

    interface Node {
      id: ID!
    }

    interface Timestamps {
      createdAt: DateTime!
      updatedAt: DateTime!
    }

    enum Gender {
      MALE
      FEMALE
    }

    input MyInput @bingo {
      foo: String! @bingo
      bar: Float
    }

    type User implements Node & Timestamps @example {
      id: ID! @example(sweet: true)
      firstName: String!
      lastName: String!
      nicknames: [String!]
      createdAt: DateTime!
      updatedAt: DateTime!
    }

    type Admin implements Entity {
      id: ID!
      badgeNumber: Int!
      createdAt: DateTime!
      updatedAt: DateTime!
    }

    type Foo {
      bar: String!
    }

    union Person = User | Admin @test
  |};

  test("should parse schema correctly", () => {
    let maybeDocument = Parser.parse(schema);
    Js.log(maybeDocument);
    let out = maybeDocument->Belt.Result.getExn->Printer.print;
    Js.log(out);
    
    expect(Belt.Result.isOk(maybeDocument)) |> toBe(true);

    /* expect(out) |> toBe(schema); TODO Some whitespace formatting issues */
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