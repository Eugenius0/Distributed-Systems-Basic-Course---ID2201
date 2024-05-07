-module(hist).
-export([new/1, update/3]).

-record(history, {name, highest_msg_number}). % history record to keep track of message numbers from received different nodes


% create a new history with an initial message number for the specified node.
new(Node) when is_atom(Node) -> % ensure that argument 'Node' is an atom
    #history{name = Node, highest_msg_number = 999999}. % set a very high initial value.


% check if a message is old or new and update the history accordingly.
update(Node, MsgNumber, History) ->
    case history_entry(Node, History) of
        {NewestMsgNumber, UpdatedHistory} when MsgNumber > NewestMsgNumber ->
            {new, set_history_entry(Node, MsgNumber, UpdatedHistory)};
        _ ->
            old
    end.

% helper function to get the highest message number for a node.
history_entry(Node, #history{name = Node, highest_msg_number = MsgNumber} = History) ->
    {MsgNumber, History};
history_entry(_Node, History) ->
    {0, History}.

% helper function to set the highest message number for a node.
set_history_entry(Node, MsgNumber, #history{name = Node, highest_msg_number = _} = History) ->
    History#history{highest_msg_number = MsgNumber};
set_history_entry(Node, MsgNumber, History) ->
    History#history{name = Node, highest_msg_number = MsgNumber}.
