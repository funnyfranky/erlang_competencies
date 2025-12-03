leftMHeaps
=====

An OTP library

Build
-----

    $ rebar3 compile


<!-- 
H0 = leftMHeaps:new().
nil

H1 = leftMHeaps:insert(H0, 10).
{1,10,nil,nil}

H2 = leftMHeaps:insert(H1, 20).
{1,10,{1,20,nil,nil},nil}

H3 = leftMHeaps:insert(H2, 5).
{2,5,{2,10,{1,20,nil,nil},nil},nil}

H4 = leftMHeaps:insert(H3, 15).
{2,5,{2,10,{1,20,nil,nil},nil},{1,15,nil,nil}}

leftMHeaps:toList(H4).
[5,10,15,20] 
-->