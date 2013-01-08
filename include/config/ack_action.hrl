ack_action() ->
  AckAction = [
    [10 | [
      {params, [
        {errorcode, integer},
        {errordesc, string}
      ]}
    ]],
    [100 | [
      {params, [
        {username, string},
        {udid, string}
      ]}
    ]],
    [101 | [
      {params, [
        {username, string},
        {age, integer}
      ]}
    ]],
    [200 | [
      {params, [
        {users, list, [
          {username, string},
          {age, integer}
        ]}
      ]}
    ]],
    [201 | [
      {params, [
        {username, string},
        {age, integer},
        {girlfriends, list, [
          {username, string},
          {age, integer}
        ]}
      ]}
    ]]
  ],
  {ok, AckAction}.
