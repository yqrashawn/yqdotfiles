function requestNotificationPermission() {
  // Check if the browser supports notifications
  if (!("Notification" in window)) {
    alert("This browser does not support desktop notification")
  }
  // Check whether notification permissions have already been granted
  else if (Notification.permission === "granted") {
    // If it's okay let's create a notification
    console.log("Permission already granted")
  }
  // Otherwise, ask the user for permission
  else if (Notification.permission !== "denied") {
    Notification.requestPermission().then(function (permission) {
      // If the user accepts, let's create a notification
      if (permission === "granted") {
        console.log("Permission granted")
      }
    })
  }
}

// Call the function to request notification permission
requestNotificationPermission()
