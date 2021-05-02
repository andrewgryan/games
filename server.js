const path = require("path")
const http = require("http")
const { Server } = require("socket.io")
const express = require("express")  
const app = express()
const port = process.env.PORT || 8080

let leaderboard = []

app.use(express.static(__dirname + "/static"))

// View engine
app.set("view engine", "ejs")


app.get("/", (req, res) => {
    // res.sendFile(path.join(__dirname, "index.html"))
    let baseUrl
    if (process.env.NODE_ENV === "production") {
        baseUrl = "https://sheltered-basin-70535.herokuapp.com"
    } else {
        baseUrl = "http://localhost:8080"
    }
    res.render(path.join(__dirname, "index.ejs"), {
        title: "Quiz",
        baseUrl
    })
})


let httpServer = http.createServer(app)

// Socket.io
const io = new Server(httpServer)
io.on("connection", socket => {
    console.log("a user connected")
    socket.on("score", msg => {
        let { payload } = JSON.parse(msg)
        leaderboard.push(payload)
        // Emit to everyone
        io.emit("leaderboard", leaderboard)
    })
})

httpServer.listen(port, () => {
    console.log(`listening on: http://localhost:${port}`)
})
