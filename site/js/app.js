MicroModal.init({
  onShow: () => {
    setTimeout(() => {
      // Delay for scrolling top on Safari
      document.querySelector(".modal__container").scrollTop = 0;
    }, 25);
  },
});
