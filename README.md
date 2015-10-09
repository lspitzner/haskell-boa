# boa
A program to check and impose constraints on imports in Haskell modules.

## What exactly does it do?
boa allows you to specify what modules your Haskell modules are allowed to import and then checks whether your specification is violated.

## Why is this useful?
A simple use case would be preventing you from using unstable parts of a library. Forbid the use of the unstable modules and you should be fine.

Getting more involved, boa could be used to assist when switching from one library to an other. Forbid the use of its modules and you will get a handy list of where you have work to do.

In bigger codebases where code is shared between multiple projects boa can be used to keep dependencies from growing out of control.

Also, it is an other drug for people addicted to static guarantees.

## Where is the catch?
boa in its current stage is essentially a proof-of-concept. You can play around with it, but don't expect it to be very usable.
