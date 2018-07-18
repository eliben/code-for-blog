# Add this code to your .bashrc to save persistent history
#
# See http://eli.thegreenplace.net/2013/06/11/keeping-persistent-history-in-bash
# for details.
#
# Note, HISTTIMEFORMAT has to be set and end with at least one space; for
# example:
#
#   export HISTTIMEFORMAT="%F %T  "
#
# If your format is set differently, you'll need to change the regex that
# matches history lines below.
#
# Eli Bendersky (http://eli.thegreenplace.net)
# This code is in the public domain

log_bash_persistent_history()
{
  local rc=$?
  [[ $(history 1) =~ ^\ *[0-9]+\ +([^\ ]+\ [^\ ]+)\ +(.*)$ ]]
  local date_part="${BASH_REMATCH[1]}"
  local command_part="${BASH_REMATCH[2]}"
  if [ "$command_part" != "$PERSISTENT_HISTORY_LAST" ]
  then
    echo $date_part "|" "$command_part" >> ~/.persistent_history
    export PERSISTENT_HISTORY_LAST="$command_part"
  fi
}

# Stuff to do on PROMPT_COMMAND
run_on_prompt_command()
{
    log_bash_persistent_history
}

if [ "$PROMPT_COMMAND" = "" ]
then
    PROMPT_COMMAND="run_on_prompt_command"
else
    PROMPT_COMMAND="run_on_prompt_command; ""$PROMPT_COMMAND"
fi
