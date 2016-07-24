type id = int

type request = Create
             | Parse id * Api_types.code
             | Start id * Api_types.parameter
             | Status id * Api_types.token
             | List id
             | Stop id *Api_types.token
type response = Create id * Api_types.error
              | Parse id * Api_types.token result
              | Status id * Api_types.state result
              | List id * Api_types.catalog result
              | Stop unit result
