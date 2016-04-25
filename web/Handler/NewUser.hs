module Handler.NewUser where

import Import
import Yesod.Auth.HashDB(setPassword)

newUserForm :: Form (Text,Text,Text)
newUserForm = renderDivs $ (\uname p1 p2 -> (uname,p1,p2)) <$>
  areq emailField "Email" Nothing <*>
  areq passwordField "Password" Nothing <*>
  areq passwordField "Confirm Password" Nothing


getNewUserR :: Handler Html
getNewUserR = do
  (userform, userenctype) <- generateFormPost newUserForm
  defaultLayout $ do
    setTitle "Create User"
    $(widgetFile "newuser")

postNewUserR :: Handler ()
postNewUserR = do
  ((result, _), _) <- runFormPost newUserForm
  case result of
   FormSuccess (uname,p1,p2) ->
     if (p1==p2)
       then do
         mu <- runDB $ getBy $ UniqueUser uname
         case mu of
          Just _ -> do
            setMessage "Username already exists"
            redirect NewUserR
          Nothing -> do
            u <- setPassword p1 $ User uname Nothing
            _ <- runDB $ insert u
            redirect HomeR
     else do
       setMessage "Passwords not equal"
       redirect NewUserR
   _ -> error "Problem with the form"
