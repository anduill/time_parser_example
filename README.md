# Time Parser Example
This project demonstrates how to make a simple grammar for parsing a time string.  There is additional functionality
to add time (minutes) to the time to get a new time.  For example:

```
addMinutes("9:13 AM", 200)
```

would return:
```
"12:33 PM"
```
## Web Service
I packaged this routine to run in a webservice for convenience.

### Build
```bash
sbt clean test assembly
```
### SBT Run
```bash
sbt run
```
### API Usage
Simply hit the following endpoint (as in this example):
```bash
http://127.0.0.1:8080/time?time=11:53%20AM&minutes=1240
```
Note that this endpoint requires escaping the space (i.e. %20)
If a improperly formatted string is passed, the service will return a BAD_REQUEST code with error message that
should provide some clues to the problem.  For example, consider the following:
```
http://localhost:8080/time?time=11:63%20AM&minutes=1240
```
The response would be:
```json
{"message":"Parser Failure, total input 11:63 AM.  With error-message: string matching regex '[0-5]' expected but '6' found"}
```