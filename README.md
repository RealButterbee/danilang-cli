# Features

## Types
Integer\
String

## Statements
// this is a comment, and can be put almost anywhere //\
let identifier = expression;\
Print expression;\
if (condition) { statements } eles { statements }\
Connect host port {message};\
External identifier {name: "binary.exe" args: "-p {port} -i {ip}" defaults: { port: "8080", ip: "127.0.0.1"} }

## Expressions
see Types
base64(<expression>)
base64_decode(<expression>)
json(<key1>, <value1>, <key2>, <value2>, ...) // returns a json string with the arguments supplied
string(var) // turns the var or expression into a string, useful for converting bytearrays etc

# Ideas

## Types
### Primitive Types
Integer\
Boolean\
String

### Compound Types
Array\

## Network operations

connect keyword defines a connection, takes some parameters and can

connect <tcp/udp> <host> <port> (this performs a one time connection, must define send and receive like below)

    connect tcp 192.168.1.1 8080 {
        send "GET / HTTP/1.1\r\nHost: 192.168.1.1\r\n\r\n".bytes();
        receive response;
        print response;
    }


