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
const random = () => (Math.random() * 8 - 4).toFixed(1);

setInterval(() => {
  dots.forEach((dot, i) => {
    const randomDelay = Math.random() * 350;
    setTimeout(() => {
      dot.style.transform = `translate(${random()}px, ${random()}px)`;
    }, randomDelay);
  });
}, 500);
