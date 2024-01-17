function _disconncedOtherHost() {
  const hostnameQ = new URL(location.href).searchParams.get("hostname")
  if (!hostnameQ) return

  const macDevices = document.querySelectorAll(".devices-page__list")?.[0]
  console.log("macDevices = ", macDevices)
  if (!macDevices) return
  const firstMacDevice = macDevices.querySelector(".device-list-item")
  console.log("firstMacDevice = ", firstMacDevice)
  if (!firstMacDevice) return
  const disconnecdButton = firstMacDevice.querySelector("button.btn")
  console.log("disconnecdButton = ", disconnecdButton)
  if (!disconnecdButton) return

  if (firstMacDevice?.innerText === "Available Mac device") return

  if (hostnameQ === "studio") {
    if (firstMacDevice.innerText.match(/studio/i)) return
    else {
      disconnecdButton.click()
      return true
    }
  } else {
    if (firstMacDevice.innerText.match(/studio/i)) {
      return disconnecdButton.click()
      return true
    } else return
  }
}

function _confirmDisconnect() {
  document
    .querySelector(".modal__dialog_opened")
    ?.querySelectorAll("button")?.[1]
    .click()
}

const interval = setInterval(() => {
  try {
    if (_disconncedOtherHost()) {
      setTimeout(_confirmDisconnect, 500)
      clearInterval(interval)
    }
  } catch (err) {
    console.error(err)
  }
}, 1000)
