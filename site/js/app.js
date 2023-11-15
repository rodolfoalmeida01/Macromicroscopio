MicroModal.init({
  onShow: () => {
    setTimeout(() => {
      // Delay for scrolling top on Safari
      document.querySelector(".modal__container").scrollTop = 0;
    }, 25);
  },
});

const enterButton = document.querySelector("#enter_button");
const intro = document.querySelector("#intro");
const cables = document.querySelector("#cables");

function start() {
  intro.classList.add("hide");
  cables.classList.remove("hide");
}

enterButton.addEventListener("click", start);
