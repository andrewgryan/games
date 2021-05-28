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
app.get("/room/*", indexHandler)


let httpServer = http.createServer(app)

// Socket.io
const io = new Server(httpServer)
io.on("connection", socket => {
    console.log("connected socket id", socket.id)

    // Wire up enter/leave events
    socket.emit("enter", { type: "enter", payload: { id: socket.id} })
    socket.on("disconnect", () => {
        socket.broadcast.emit("exit", { type: "exit", payload: { id: socket.id} })
    })

    // Listen for user join
    // TODO move this to client layer
    socket.on("join", msg => {
        console.log("join: ", msg)
    })

    // Listen for answer
    // TODO move this to client layer
    socket.on("answer", msg => {
        console.log("answer: ", msg)
    })

    // Public & private message(s)
    socket.on("private", msg => {
        io.to(msg.to).emit("private", msg)
    })
    socket.on("public", msg => {
        socket.broadcast.emit("public", msg)
    })

    // Listen for score
    // TODO move this to client layer
    socket.on("score", payload => {
        leaderboard.push(payload)
        // Emit to everyone
        io.emit("leaderboard", leaderboard)
    })
})

httpServer.listen(port, () => {
    console.log(`listening on: http://localhost:${port}`)
})
