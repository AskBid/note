As seen on [YouTube video](https://www.youtube.com/watch?v=omSpI1Nu_pg&ab_channel=TheDumbfounds)

Every query starts with curly brackets

```
{

}
```

In Schema you may have

```js
allPeople(
    after: String
    first: Int
    before: String
    last: Int
): PeopleConnection
```

Clicking on `PeopleConnection` (in cardano it seem to see PeopleConnection fields you have to check it from DOCS rather than Schema) you'll get the attributes of allPeople you can get back. For instance if in `PeopleConnection` you have attributes like `totalCount` and `people`.
Inside the brackets is instead the optional parameters for our query.

```js
{
    allPeople {
        totalCount
    }
}

> 

{
    "data": {
        "allPeople": {
            "totalCount": 87
        }
    }
}
```

```js
{
    allPeople {
        people
    }
}
```

This would give an error because unlike `totalCount`, `people` requires a field.

```js
{
    allPeople {
        people {
            name
        }
    }
}

> 

{
    "data": {
        "allPeople": {
            "people": [
                {
                    "name": "Giacomo"
                },
                {
                    "name": "Laura"
                }
                ...
            ]
        }
    }
}
```

Now let's try to use a parameter


```js
{
    allPeople(limit: 1) {
        people {
            name
        }
    }
}

> 

{
    "data": {
        "allPeople": {
            "people": [
                {
                    "name": "Giacomo"
                }
            ]
        }
    }
}
```

GraphQL can be divided in **queries** and **mutations**
We can name a query like this

```js
query FirstPerson {
    allPeople(limit: 1) {
        people {
            name
        }
    }
}
```

One avantage of Gql is that you can get whatever you want with just one request and here is an example

```js
query firstAndLastTwoPeople {
    first2people:
        allPeople(first: 2) {
            people {
                name
            }
        }
    last2people:
        allPeople(last: 2) {
            people {
                name
            }
        } 
}

> 

{
    "data": {
        "first2people": {
            "people": [
                {
                    "name": "Giacomo"
                },
                {
                    "name": "Laura"
                }
            ]
        }
        "last2people": {
            "people": [
                {
                    "name": "Fester"
                },
                {
                    "name": "Rania"
                }
            ]
        }
    }
}
```

Imagine now htat instead of just `name` we wanted more fields as `eyeColor`, `birthDate`.. we would have a repetition.
That's when **`fragments`** come in handy

```js
query firstAndLastTwoPeople {
    first2people:
        allPeople(first: 2) {
            people {
                ...peopleFields
            }
        }
    last2people:
        allPeople(last: 2) {
            people {
                ...peopleFields
            }
        } 
}

// considering that people is of type people:[Person]

fragment peopleFields on Person {
    name
    eyeColor
    birthDate
}
```

And now with variables

```js
query firstAndLastNpeople($n: Int = 2) {
    firstNpeople:
        allPeople(first: $n) {
            people {
                ...peopleFields
            }
        }
    lastNpeople:
        allPeople(last: $n) {
            people {
                ...peopleFields
            }
        } 
}
```

And now some filtering option with **`@include`** or **`@skip`**

```js
query firstAndLastNpeople($n: Int = 2, $someBoolean: Boolean = True) {
    firstNpeople:
        allPeople(first: $n) {
            people @include(if: $someBoolean) {
                ...peopleFields
            }
        }
    lastNpeople:
        allPeople(last: $n) {
            people {
                ...peopleFields
            }
        } 
}
```
