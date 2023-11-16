MicroModal.init({
  onShow: () => {
    setTimeout(() => {
      // Delay for scrolling top on Safari
      document.querySelector(".modal__container").scrollTop = 0;
    }, 25);
  },
});

// Intro
const enterButton = document.querySelector("#enter_button");
const intro = document.querySelector("#intro");
const cables = document.querySelector("#cables");

function start() {
  intro.classList.add("hide");
  cables.classList.remove("hide");
}

enterButton.addEventListener("click", start);

// Move intro dots
const dots = document.querySelectorAll(".float");
const random = () => [3, 2, 1, 0, -1, -2, -3][Math.floor(Math.random() * 3)];

setInterval(() => {
  dots.forEach((dot) => {
    dot.style.transform = `translate(${random()}px, ${random()}px)`;
  });
}, 750);
