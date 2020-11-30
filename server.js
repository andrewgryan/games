const path = require("path")
const express = require("express")  
const app = express()
const port = process.env.PORT || 8080


app.use(express.static(__dirname + "/static"))


app.get("/", (req, res) => {
    res.sendFile(path.join(__dirname, "index.html"))
})


app.listen(port, () => {
    console.log(`listening on: http://localhost:${port}`)
})
