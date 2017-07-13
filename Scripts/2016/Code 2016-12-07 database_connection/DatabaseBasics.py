
import tkinter as TK
import sqlalchemy as SQL

import pandas as PD


"""
#######################################################################
### Database Interaction                                            ###
#######################################################################
"""

sql_credentials = { \
                  'user': 'sloth' \
                , 'database': 'Bones' \
                }


#______________________________________________________________________
### Password Prompt ###
"""
http://stackoverflow.com/questions/15724658/simplest-method-of-asking-user-for-password-using-graphical-dialog-in-python
"""
def PasswordPrompt(label = 'please enter password:', header = 'Password'):
    prompt = TK.Tk()
    prompt.wm_title(header)
    TK.Label(prompt, text = label).pack(side = 'top')

    pwd = TK.StringVar()
    pwdbox = TK.Entry(master = prompt, textvariable=pwd, show = '*')

    pwdbox.pack(side = 'top')
    pwdbox.bind('<Return>', lambda _: prompt.destroy())
    TK.Button(prompt, command=lambda: prompt.destroy(), text = 'OK').pack(side = 'top')

    prompt.mainloop()

    return pwd.get()

#______________________________________________________________________
### SQL connection ###
class SQLConnect(dict):
    """
    Encapsulate some common information required to run the experiment
    allow instantiation with a default set.
    from http://stackoverflow.com/a/9550596
    Data storage by subclassing dict

    """
    
    def __init__(self, credentials=None): 
        # first initialize all mandatory values.
        self.SetDefaults()
        # then overwrite everything the user provides.
        if credentials is not None:
            for d in credentials.keys():
                self[d] = credentials[d]

        if self.get('password') is None:
            self['password'] = PasswordPrompt( \
                                          label = 'please enter password for \n %s @ %s' % (self['user'], self['hostname']) \
                                        , header = 'database password')

        # here the connection string is formed from all the information
        self.engine = SQL.create_engine( '%s://%s:%s@%s:%i/%s' \
                    % (   self['dialect'] \
                        , self['user'] \
                        , self['password'] \
                        , self['hostname'] \
                        , self['port'] \
                        , self['database'] \
                        ) )

    def __getattr__(self, param):
        return self[param]

    def __setattr__(self, param, value):
        self[param] = value

    def SetDefaults(self):
        self['hostname'] = 'localhost' # '141.20.60.187' # 
        self['port'    ] =  5432
        self['dialect' ] = 'postgresql+psycopg2'

    def Close(self):
        self.engine.dispose()




"""
#######################################################################
### data query                                                      ###
#######################################################################
"""
def DataQuery(query):
        
    db = SQLConnect(sql_credentials)

    data = PD.read_sql_query( \
                                query \
                                , db.engine \
                                )

    db.Close()
    return data

    