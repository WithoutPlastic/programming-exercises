#include <node.h>
#include <v8.h>

using namespace v8;

//Implement Predefined Method
Handle<Value> SayHello(const Arguments& args) {
    HandleScope scope;
    return scope.Close(String::New("Hello world!"));
}

//Add sayHello() To Target Object
void Init_Hello(Handle<Object> target) {
    target->Set(String::NewSymbol("sayHello"), FunctionTemplate::New(SayHello)->GetFunction());
}

//Call NODE_MODULE To Register Methods In Memory
NODE_MODULE(hello, Init_Hello)
