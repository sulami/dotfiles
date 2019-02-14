# Are we currently located in a stack env? Mind the retval.
in_stack_dir()
{
    stack query &> /dev/null
}

