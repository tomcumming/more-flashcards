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

  /** @argument isQuiz {boolean} @argument char {string} @argument word {string} */
  function makeWord(isQuiz, char, word) {
    return Array.from(word)
      .map(
        (c) => `<span
        class="${c === char ? "so-current" : ""}"
        >${c === char && isQuiz ? "？" : c}</span>`,
      )
      .join("");
  }

  document.querySelectorAll(".so-info").forEach((infoElem) => {
    const wordsAttr = infoElem.getAttribute("data-words");
    const pinyinAttr = infoElem.getAttribute("data-pinyin");
    const englishAttr = infoElem.getAttribute("data-english");
    const char = infoElem.getAttribute("data-char");
    const isQuiz = infoElem.hasAttribute("data-quiz");

    if (
      wordsAttr === null ||
      pinyinAttr === null ||
      englishAttr === null ||
      char === null
    )
      reportError("Cant get attrs on .so-character");

    const words = wordsAttr.split("，");
    const pinyin = pinyinAttr.split("，");
    const english = englishAttr.split("，");

    words.forEach((word, idx) => {
      const item = document.createElement("div");
      item.innerHTML = `<div>${makeWord(isQuiz, char, word)}</div>
          <div>${pinyin[idx]}</div>
          <div>${english[idx]}</div>`;
      infoElem.appendChild(item);
    });
  });

  let mistakes = new Set();
  let totalCount = 1;

  document.querySelectorAll(".so-quiz").forEach((quizElem) => {
    if (!(quizElem instanceof HTMLElement))
      reportError("quizElem not HTMLElement");

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
      markStrokeCorrectAfterMisses: 2,

      onLoadCharDataSuccess: ({ strokes }) => {
        totalCount = strokes.length;
      },

      onMistake: ({ strokeNum }) => {
        mistakes.add(strokeNum);
      },

      onComplete: ({}) => {
        const mistakeCount = mistakes.size;
        const correctCount = totalCount - mistakeCount;
        if (mistakeCount === 0) {
          resultElem.classList.add("so-perfect");
          resultElem.appendChild(new Text("Perfect!"));
        } else {
          const percent = (correctCount / totalCount) * 100;
          resultElem.classList.add(percent < 80 ? "so-errors" : "so-1error");
          resultElem.appendChild(
            new Text(`${mistakeCount} mistake(s) - ${percent.toFixed(0)}%`),
          );
        }
      },
    };

    const handle = hw.create(quizElem, char, options);
    handle.quiz();
  });
  document.querySelectorAll(".so-answer-animation").forEach((animElem) => {
    if (!(animElem instanceof HTMLElement))
      reportError("animElem not HTMLElement");

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

// syncing issue?
