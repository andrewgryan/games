const path = require("path")
const http = require("http")
const { Server } = require("socket.io")
const express = require("express")  
const app = express()
const port = process.env.PORT || 8080

let leaderboard = []

app.use(express.static(__dirname + "/dist"))


let indexHandler = (req, res) => {
    res.sendFile(path.join(__dirname, "dist", "index.html"))
}
app.get("/", indexHandler)
app.get("/quiz", indexHandler)
app.get("/new", indexHandler)


let httpServer = http.createServer(app)

// Socket.io
const io = new Server(httpServer)
io.on("connection", socket => {
    console.log("A user connected")

    // Listen for answer
    socket.on("answer", msg => {
        console.log(msg)
    })

    // Listen for score
    socket.on("score", payload => {
        leaderboard.push(payload)
        // Emit to everyone
        io.emit("leaderboard", leaderboard)
    })
})

httpServer.listen(port, () => {
    console.log(`listening on: http://localhost:${port}`)
})
