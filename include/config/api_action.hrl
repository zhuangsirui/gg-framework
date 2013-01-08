route() ->
  ApiRoute = [
    [1000 | [
      {controller, user_controller},
      {action, user_register},
      {params, [
        {username, string},
        {udid, string}
      ]}
    ]],
    [1001 | [
      {controller, user_controller},
      {action, user_login},
      {params, [
        {username, string},
        {udid, string}
      ]}
    ]],
    [1002 | [
      {controller, user_controller},
      {action, login},
      {params, [
        {username, string},
        {age, integer}
      ]}
    ]],
    [1003 | [
      {controller, user_controller},
      {action, login},
      {params, [
        {age, integer},
        {username, string}
      ]}
    ]],
    [1004 | [
      {controller, user_controller},
      {action, login},
      {params, [
        {age, integer},
        {username, string},
        {password, string},
        {vip_rank, integer}
      ]}
    ]],
    [1005 | [
      {controller, user_controller},
      {action, user_logout},
      {params, [
      ]}
    ]],
    [1006 | [
      {controller, user_controller},
      {action, user_info},
      {params, [
      ]}
    ]],
    [2000 | [
      {controller, building},
      {action, build},
      {params, [
        {users, list, [
          {username, string}
        ]}
      ]}
    ]],
    [2001 | [
      {controller, building},
      {action, build},
      {params, [
        {users, list, [
          {age, integer}
        ]}
      ]}
    ]],
    [2002 | [
      {controller, building},
      {action, build},
      {params, [
        {users, list, [
          {age, integer},
          {nickname, string}
        ]}
      ]}
    ]]
  ],
  {api, ApiRoute}.
