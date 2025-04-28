# Elm Requests

`elm-requests` makes complex HTTP requests simpler:

* HTTP requests can be created bit by bit with a pipeline API.
* HTTP requests can be easily called in parallel.
* HTTP requests can be chained, so the result of one request automatically 
  construct another request.
* Per-app configurations are stored in a global config so you don't have to 
  repeat everything again on each request.

## Examples

In Elm, a typical request will be 

```elm
import Http

Http.get 
    { url = "http://deep-thought.com/api/v1/questions/ultimate"
    , expect = Http.expectJson ReceivedAnswer D.int
    }
```

Requests works similarly

```elm
import Request

Request.get 
    { url = "http://deep-thought.com/api/v1/questions/ultimate"
    , expect = Request.expectJson D.int
    }
```

Notice that the message is not part of the request. `Request.get` also do not 
return a Cmd msg, but rather a `Request data` object. We can execute it 
immediately by calling the function

```elm
Request.cmd simple ReceivedAnswer request 
    ==> Cmd
```

and obtain the same behavior as in Elm's native Http.get.

This little indirection has a huge payoff. We can now create request objects
more easily using a pipeline interface. They can also be easily converted to tasks 
which allows chaining, batch execution, and more.

For instance, we can chain two http requests so our application only need to 
handle the final response:

```elm
getAnswer : Request Int
getAnswer = Request.get 
    { url = "http://deep-thought.com/api/v1/questions/ultimate"
    , expect = Request.expectJson D.int
    }

getQuestion : Int -> Request String
getQuestion answer = Request.get 
    { url = "http://deep-thought.com/api/v1/answers/" ++ String.fromInt
    , expect = Request.expectJson D.string
    }

Request.task simple getAnswer
    |> Task.andThen 
        (\answer -> 
            Request.task simple getQuestion
        )
    |> Task.attempt QuestionReceived
```
