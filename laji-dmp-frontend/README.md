# Development
```
npm i

npm start
```

To test login (the app has not been registered to laji-auth yet):
```
http://localhost:8000/login?access_token=<your-apitest-token>&next=/dmp
```

Known issues:
- CSS cache busting doesn't work. You may have to manually delete browser CSS cache sometimes...
