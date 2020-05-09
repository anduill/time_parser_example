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
http://127.0.0.1:8080/time/getTime?startingTime="9:13 AM"&mins=200
```
If a improperly formatted string is passed, the service will return a BAD_REQUEST code with
the following error message:
```json
{
    "message": "Parsing failure for unit string <BAD STRING>",
    "exception": "No result when parsing failed"
}
```