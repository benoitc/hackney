

# Module hackney_disp_handler #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Dispcount worker implementation for TCP socket handling.
__Behaviours:__ [`coffer_pool_handler`](coffer_pool_handler.md), [`dispcount`](/Users/benoitc/work/hackney/deps/dispcount/doc/dispcount.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#checkin-2">checkin/2</a></td><td></td></tr><tr><td valign="top"><a href="#checkout-2">checkout/2</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#dead-1">dead/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="checkin-2"></a>

### checkin/2 ###

`checkin(Socket, State) -> any()`


<a name="checkout-2"></a>

### checkout/2 ###

`checkout(From, State) -> any()`


<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`


<a name="dead-1"></a>

### dead/1 ###

`dead(State) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Msg, State) -> any()`


<a name="init-1"></a>

### init/1 ###

`init(Init) -> any()`


<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


