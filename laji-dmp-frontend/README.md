# Development
Setup dev server:
```
npm i -g elm-live
```

Host dev server (the quotes in "--" are required in windows):

```
elm-live src/Main.elm --pushstate --start-page=src/index.html "--" --output=dist/elm.js
```

To test login (the app has not been registered to laji-auth yet):
```
http://localhost:8000/login?access_token=<your-apitest-token>&next=/dmp
```
