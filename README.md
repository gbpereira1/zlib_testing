app1
=====

An OTP application to play around with zlib and
passing the data between process

Usage
-----

    $ rebar3 shell
    1> {ok,Pid1} = app1:start_link().
        
    2> {ok,Pid2} = app2:start_link().
    
    3> app1:zip_and_send(Pid2).
    The size of bin osa is:32000
    ok
    The size of Data is:9331
    App2 received something, let see...
    Received: 32000 bytes
    It's the fake OSA!
