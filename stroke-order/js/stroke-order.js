/** @import HanziWriter, {HanziWriterOptions} from 'hanzi-writer' */

{
  /** @type {typeof HanziWriter} */
  // @ts-ignore
  const hw = window.HanziWriter;

  const [canvasWidth, canvasHeight] = [300, 300];

  const resultElem = document.querySelector(".so-result");
  if (!resultElem) throw new Error();

  /** @param {string} error 
  @returns {never}*/
  function reportError(error) {
    resultElem?.appendChild(new Text(error));
    throw new Error();
  }

  document.querySelectorAll(".so-characters").forEach((charsElem) => {
    const word = charsElem.getAttribute("data-word");
    const char = charsElem.getAttribute("data-char");
    const idxTxt = charsElem.getAttribute("data-idx");

    if (word === null || idxTxt === null)
      reportError("Cant get attrs on .so-character");

    const idx = parseInt(idxTxt);

    {
      const chars = Array.from(word);

      chars.forEach((c, i) => {
        const span = document.createElement("span");
        span.innerText = c === char ? "？" : c;
        span.className = i == idx ? "so-current-char" : "";
        charsElem.appendChild(span);
      });
    }
  });

  document.querySelectorAll(".so-quiz").forEach((quizElem) => {
    const char = quizElem.getAttribute("data-char");
    if (char === null) reportError("Can't read quiz char");

    /** @type {Partial<HanziWriterOptions>} */
    const options = {
      width: canvasWidth,
      height: canvasHeight,
      showCharacter: false,
      padding: 5,
      showOutline: false,
      showHintAfterMisses: 1,

      onComplete: ({ totalMistakes }) => {
        if (totalMistakes === 0) {
          resultElem.classList.add("so-perfect");
          resultElem.appendChild(new Text("Perfect!"));
        } else {
          resultElem.classList.add(
            totalMistakes > 1 ? "so-errors" : "so-1error",
          );
          resultElem.appendChild(new Text(`${totalMistakes} error(s)`));
        }
      },
    };

    const handle = hw.create(quizElem, char, options);
    handle.quiz();
  });
  document.querySelectorAll(".so-answer-animation").forEach((animElem) => {
    const char = animElem.getAttribute("data-char");
    if (char === null) reportError("Can't read anim char");

    /** @type {Partial<HanziWriterOptions>} */
    const options = {
      width: canvasWidth,
      height: canvasHeight,
      delayBetweenStrokes: 0,
      delayBetweenLoops: 0,
    };

    const handle = hw.create(animElem, char, options);
    handle.loopCharacterAnimation();
  });
}
