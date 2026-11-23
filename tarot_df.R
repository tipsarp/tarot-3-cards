deck_df <- data.frame(
  Name = c("The Fool", "The Magician", "The High Priestess", "The Empress", "The Emperor", 
           "The Hierophant", "The Lovers", "The Chariot", "Strength", "The Hermit", 
           "Wheel of Fortune", "Justice", "The Hanged Man", "Death", "Temperance", 
           "The Devil", "The Tower", "The Star", "The Moon", "The Sun", "Judgement", "The World"),
  Number = 0:21,
  Element = c("Air", "Air", "Water", "Earth", "Fire", 
              "Earth", "Air", "Water", "Fire", "Earth", 
              "Fire", "Air", "Water", "Water", "Fire", 
              "Earth", "Fire", "Air", "Water", "Fire", 
              "Fire", "Earth"),
  Astrology = c("Uranus", "Mercury", "Moon", "Venus", "Aries", 
                "Taurus", "Gemini", "Cancer", "Leo", "Virgo", 
                "Jupiter", "Libra", "Neptune", "Scorpio", "Sagittarius", 
                "Capricorn", "Mars", "Aquarius", "Pisces", "Sun", 
                "Pluto", "Saturn"),
  Meaning = c("New beginnings, innocence, spontaneity", "Manifestation, resourcefulness, power", 
              "Intuition, sacred knowledge, divine feminine", "Femininity, beauty, nature, abundance", 
              "Authority, establishment, structure", "Spiritual wisdom, religious beliefs, tradition", 
              "Love, harmony, relationships, values", "Control, willpower, success, action", 
              "Strength, courage, persuasion, patience", "Soul searching, introspection, being alone", 
              "Good luck, karma, life cycles, destiny", "Justice, fairness, truth, cause and effect", 
              "Pause, surrender, letting go, new perspective", "Endings, change, transformation, transition", 
              "Middle path, patience, finding meaning", "Shadow self, addiction, restriction, hopelessness", 
              "Disaster, sudden change, upheaval, awakening", "Hope, faith, purpose, renewal, spirituality", 
              "Illusion, fear, anxiety, subconscious", "Positivity, fun, warmth, success, vitality", 
              "Judgement, rebirth, inner calling, absolution", "Completion, accomplishment, travel, fulfillment"),
  stringsAsFactors = FALSE
)

suits <- c(Wands = "Wands", Cups = "Cups", Swords = "Swords", Pentacles = "Pents")

element_info <- c(
  Fire  = "Energy, passion, will, ambition and creative drive",
  Water = "Emotion, intuition, relationships and the subconscious",
  Air   = "Intellect, communication, truth, decisions and conflict",
  Earth = "The material world: body, work, money, home and stability"
)

element_dominant_msg <- c(
  Fire  = "Fire dominates: this situation calls for action, courage and creative initiative.",
  Water = "Water dominates: feelings, relationships and intuition sit at the heart of this matter.",
  Air   = "Air dominates: thoughts, decisions and communication hold the key - beware of overthinking.",
  Earth = "Earth dominates: practical concerns - work, health, money and tangible results - take centre stage."
)

# ---- Numerology helpers -------------------------------------------------------
numerology_base <- c(
  "1"  = "Beginnings, initiative, individuality and raw potential",
  "2"  = "Duality, balance, partnership and choice",
  "3"  = "Creativity, growth, expression and collaboration",
  "4"  = "Stability, structure, foundation and order",
  "5"  = "Change, conflict, instability and challenge",
  "6"  = "Harmony, communication, cooperation and healing",
  "7"  = "Reflection, assessment, mystery and inner work",
  "8"  = "Mastery, movement, power and accomplishment",
  "9"  = "Culmination, attainment, wisdom and near-completion",
  "10" = "Completion, endings, renewal and the close of a cycle"
)

court_numerology <- c(
  "Page (11) - The student and messenger: curiosity, new learning and fresh news",
  "Knight (12) - The seeker in motion: action, pursuit and the extremes of the suit",
  "Queen (13) - Inner mastery: nurturing, receptive embodiment of the suit",
  "King (14) - Outer mastery: authority, leadership and command of the suit's energy"
)

reduce_digits <- function(n) {
  while (n > 9) n <- sum(as.integer(strsplit(as.character(n), "")[[1]]))
  n
}

major_numerology <- function(n) {
  if (n == 0) return("0 - The void and infinite potential; the unwritten beginning")
  if (n <= 9) return(paste0(n, " - ", numerology_base[[as.character(n)]]))
  r <- reduce_digits(n)
  paste0(n, " reduces to ", r, " - ", numerology_base[[as.character(r)]])
}

minor_numerology <- function(n) {
  vapply(n, function(k) {
    if (k <= 10) paste0(k, " - ", numerology_base[[as.character(k)]])
    else court_numerology[[k - 10]]
  }, character(1))
}

to_roman <- function(n) if (n == 0) "0" else as.character(utils::as.roman(n))

# ---- Major Arcana -------------------------------------------------------------
major_names <- c(
  "The Fool", "The Magician", "The High Priestess", "The Empress", "The Emperor",
  "The Hierophant", "The Lovers", "The Chariot", "Strength", "The Hermit",
  "Wheel of Fortune", "Justice", "The Hanged Man", "Death", "Temperance",
  "The Devil", "The Tower", "The Star", "The Moon", "The Sun", "Judgement", "The World"
)

major_upright <- c(
  "New beginnings, innocence, spontaneity, a leap of faith",
  "Manifestation, resourcefulness, skill, focused willpower",
  "Intuition, sacred knowledge, the subconscious, inner voice",
  "Abundance, nurturing, fertility, creativity, nature",
  "Authority, structure, stability, leadership, protection",
  "Tradition, spiritual wisdom, institutions, mentorship",
  "Love, union, harmony, alignment of values, meaningful choice",
  "Willpower, determination, victory, self-control, momentum",
  "Courage, compassion, inner strength, gentle mastery",
  "Introspection, solitude, inner guidance, soul-searching",
  "Cycles, fate, turning points, destiny, good fortune",
  "Fairness, truth, law, cause and effect, accountability",
  "Surrender, pause, letting go, a new perspective",
  "Endings, transformation, transition, clearing the old",
  "Balance, moderation, patience, purpose, alchemy",
  "Shadow self, attachment, addiction, materialism, restriction",
  "Sudden upheaval, revelation, awakening, collapse of false structures",
  "Hope, renewal, faith, inspiration, serenity",
  "Illusion, intuition, dreams, uncertainty, the unconscious",
  "Joy, success, vitality, clarity, positivity",
  "Rebirth, inner calling, reckoning, absolution",
  "Completion, integration, accomplishment, wholeness, travel"
)

major_reversed <- c(
  "Recklessness, naivety, foolish risk, holding back",
  "Manipulation, untapped talent, trickery, poor planning",
  "Secrets, disconnected intuition, withdrawal, repressed feelings",
  "Dependence, creative block, smothering, neglecting self-care",
  "Domination, rigidity, excessive control, lack of discipline",
  "Rebellion, personal beliefs, challenging the status quo",
  "Disharmony, imbalance, misaligned values, avoidance of choice",
  "Lack of direction, aggression, loss of control, opposition",
  "Self-doubt, weakness, insecurity, raw emotion unchecked",
  "Isolation, loneliness, withdrawal, refusing guidance",
  "Bad luck, resistance to change, breaking cycles",
  "Unfairness, dishonesty, lack of accountability, bias",
  "Stalling, resistance, needless sacrifice, indecision",
  "Resistance to change, stagnation, fear of endings",
  "Imbalance, excess, impatience, lack of long-term vision",
  "Release, breaking free, reclaiming power, detachment",
  "Averting disaster, fear of change, delayed collapse",
  "Despair, disconnection, lack of faith, discouragement",
  "Release of fear, confusion lifting, repressed truth emerging",
  "Temporary sadness, blocked joy, overly optimistic outlook",
  "Self-doubt, ignoring the call, harsh self-judgement",
  "Incomplete closure, shortcuts, delays, lack of resolution"
)

major_element <- c("Air", "Air", "Water", "Earth", "Fire", "Earth", "Air", "Water",
                   "Fire", "Earth", "Fire", "Air", "Water", "Water", "Fire", "Earth",
                   "Fire", "Air", "Water", "Fire", "Fire", "Earth")

major_astro <- c(
  "Planet: Uranus", "Planet: Mercury", "Planet: Moon", "Planet: Venus",
  "Zodiac: Aries", "Zodiac: Taurus", "Zodiac: Gemini", "Zodiac: Cancer",
  "Zodiac: Leo", "Zodiac: Virgo", "Planet: Jupiter", "Zodiac: Libra",
  "Planet: Neptune", "Zodiac: Scorpio", "Zodiac: Sagittarius", "Zodiac: Capricorn",
  "Planet: Mars", "Zodiac: Aquarius", "Zodiac: Pisces", "Planet: Sun",
  "Planet: Pluto", "Planet: Saturn"
)

# Astrological glyphs (\uFE0E forces text rather than emoji presentation)
major_glyph <- paste0(c(
  "\u2645", "\u263F", "\u263D", "\u2640", "\u2648", "\u2649", "\u264A", "\u264B",
  "\u264C", "\u264D", "\u2643", "\u264E", "\u2646", "\u264F", "\u2650", "\u2651",
  "\u2642", "\u2652", "\u2653", "\u2609", "\u2647", "\u2644"
), "\uFE0E")

majors <- data.frame(
  name       = major_names,
  arcana     = "Major Arcana",
  suit       = "Trumps (Major)",
  rank       = vapply(0:21, to_roman, character(1)),
  number     = 0:21,
  upright    = major_upright,
  reversed   = major_reversed,
  element    = major_element,
  astrology  = major_astro,
  numerology = vapply(0:21, major_numerology, character(1)),
  glyph      = major_glyph,
  keywords   = "Archetypal forces, the soul's journey and major life lessons",
  stringsAsFactors = FALSE
)

# ---- Minor Arcana -------------------------------------------------------------
rank_names <- c("Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight",
                "Nine", "Ten", "Page", "Knight", "Queen", "King")

suit_meta <- list(
  Wands     = list(element = "Fire",  glyph = "\u2663",
                   keywords = "Passion, inspiration, ambition, career and creative energy"),
  Cups      = list(element = "Water", glyph = "\u2665",
                   keywords = "Emotions, love, relationships, intuition and imagination"),
  Swords    = list(element = "Air",   glyph = "\u2660",
                   keywords = "Thought, communication, conflict, truth and decisions"),
  Pentacles = list(element = "Earth", glyph = "\u2666",
                   keywords = "Money, work, health, home and material manifestation")
)

mm <- function(...) matrix(c(...), ncol = 2, byrow = TRUE)  # upright | reversed

wands_m <- mm(
  "Inspiration, new venture, creative spark, potential", "Delays, lack of motivation, creative blocks",
  "Planning, future vision, decisions, discovery", "Fear of change, poor planning, playing it safe",
  "Expansion, foresight, progress, opportunities abroad", "Obstacles, delays, lack of foresight",
  "Celebration, homecoming, harmony, milestones", "Instability at home, cancelled plans, transition",
  "Competition, conflict, rivalry, clashing opinions", "Avoiding conflict, inner tension, truce",
  "Victory, public recognition, success, confidence", "Ego, fall from grace, lack of recognition",
  "Defence, perseverance, standing your ground", "Overwhelm, giving up, defensiveness",
  "Swift action, movement, news, momentum", "Delays, frustration, scattered energy",
  "Resilience, persistence, last stand, boundaries", "Exhaustion, paranoia, stubbornness",
  "Burden, responsibility, hard work, overcommitment", "Delegation, release, collapse under weight",
  "Enthusiasm, exploration, free spirit, good news", "Hasty decisions, lack of direction, setbacks",
  "Energy, passion, adventure, bold action", "Recklessness, frustration, haste, impulsiveness",
  "Courage, confidence, warmth, determination", "Jealousy, insecurity, demanding behaviour",
  "Leadership, vision, entrepreneurship, honour", "Impulsiveness, overbearing, unrealistic expectations"
)

cups_m <- mm(
  "New love, compassion, emotional awakening, intuition", "Blocked emotions, emptiness, repressed feelings",
  "Partnership, mutual attraction, unity", "Imbalance, broken bond, miscommunication",
  "Friendship, celebration, community", "Overindulgence, gossip, isolation",
  "Apathy, contemplation, missed opportunity", "New awareness, acceptance, moving on",
  "Grief, loss, regret, focusing on the negative", "Acceptance, forgiveness, moving forward",
  "Nostalgia, childhood memories, innocence", "Living in the past, unrealistic idealism",
  "Choices, illusion, wishful thinking", "Clarity, alignment, sober decision",
  "Walking away, seeking deeper meaning, disillusionment", "Fear of change, aimless drifting, stagnation",
  "Contentment, wishes fulfilled, satisfaction", "Smugness, dissatisfaction, materialism",
  "Emotional fulfilment, family harmony, bliss", "Broken home, misaligned values, disconnection",
  "Creative opportunity, curiosity, intuitive message", "Emotional immaturity, creative block",
  "Romance, charm, following the heart, idealism", "Moodiness, unrealistic expectations, disappointment",
  "Compassion, emotional security, deep intuition", "Codependence, emotional insecurity, martyrdom",
  "Emotional balance, diplomacy, generosity", "Manipulation, moodiness, emotional volatility"
)

swords_m <- mm(
  "Clarity, breakthrough, truth, sharp mind", "Confusion, misinformation, clouded judgement",
  "Indecision, stalemate, difficult choices", "Information overload, released tension, decision made",
  "Heartbreak, sorrow, painful truth", "Recovery, forgiveness, releasing pain",
  "Rest, recuperation, contemplation", "Restlessness, burnout, stagnation",
  "Conflict, defeat, winning at all costs", "Reconciliation, making amends, lingering resentment",
  "Transition, moving on, leaving behind", "Resistance to change, unfinished business",
  "Deception, strategy, getting away with something", "Confession, conscience, coming clean",
  "Restriction, self-imposed limits, victim mentality", "Liberation, new perspective, self-acceptance",
  "Anxiety, worry, sleepless nights", "Hope returning, reaching out, despair easing",
  "Painful endings, betrayal, rock bottom", "Recovery, regeneration, resisting an inevitable end",
  "Curiosity, new ideas, vigilance, communication", "Gossip, deception, all talk and no action",
  "Ambition, fast thinking, drive, assertiveness", "Restlessness, lack of focus, burnout",
  "Independence, clear boundaries, direct communication", "Coldness, bitterness, harsh judgement",
  "Intellectual power, authority, truth, ethics", "Manipulation, tyranny, abuse of power"
)

pentacles_m <- mm(
  "Opportunity, prosperity, manifestation, new venture", "Lost opportunity, poor planning, scarcity",
  "Balance, adaptability, juggling priorities", "Overwhelm, disorganisation, overcommitment",
  "Teamwork, collaboration, skill, building", "Disharmony, poor teamwork, mediocrity",
  "Security, control, saving, conservatism", "Greed, materialism, learning to let go",
  "Hardship, loss, isolation, insecurity", "Recovery, help arriving, hardship ending",
  "Generosity, charity, sharing, giving and receiving", "Strings attached, debt, one-sided giving",
  "Patience, long-term view, investment, assessment", "Impatience, poor returns, lack of reward",
  "Diligence, craftsmanship, mastery, skill development", "Perfectionism, lack of focus, shortcuts",
  "Abundance, luxury, self-sufficiency", "Overworking, financial setbacks, hollow success",
  "Wealth, legacy, family, long-term success", "Financial failure, lost legacy, family disputes",
  "Ambition, study, manifestation, a new skill", "Procrastination, lack of progress, learning from failure",
  "Hard work, routine, reliability, patience", "Laziness, boredom, stagnation, perfectionism",
  "Nurturing, practicality, abundance, security", "Self-care neglect, work-home imbalance",
  "Wealth, leadership, discipline, security", "Greed, stubbornness, indulgence"
)

# Golden Dawn decan & court correspondences
wands_astro <- c("Root of the powers of Fire (Aries, Leo, Sagittarius)",
                 "Mars in Aries", "Sun in Aries", "Venus in Aries", "Saturn in Leo", "Jupiter in Leo",
                 "Mars in Leo", "Mercury in Sagittarius", "Moon in Sagittarius", "Saturn in Sagittarius",
                 "Earth of Fire", "Fire of Fire - Scorpio/Sagittarius", "Water of Fire - Pisces/Aries",
                 "Air of Fire - Cancer/Leo")
cups_astro <- c("Root of the powers of Water (Cancer, Scorpio, Pisces)",
                "Venus in Cancer", "Mercury in Cancer", "Moon in Cancer", "Mars in Scorpio", "Sun in Scorpio",
                "Venus in Scorpio", "Saturn in Pisces", "Jupiter in Pisces", "Mars in Pisces",
                "Earth of Water", "Fire of Water - Aquarius/Pisces", "Water of Water - Gemini/Cancer",
                "Air of Water - Libra/Scorpio")
swords_astro <- c("Root of the powers of Air (Gemini, Libra, Aquarius)",
                  "Moon in Libra", "Saturn in Libra", "Jupiter in Libra", "Venus in Aquarius",
                  "Mercury in Aquarius", "Moon in Aquarius", "Jupiter in Gemini", "Mars in Gemini",
                  "Sun in Gemini", "Earth of Air", "Fire of Air - Taurus/Gemini", "Water of Air - Virgo/Libra",
                  "Air of Air - Capricorn/Aquarius")
pentacles_astro <- c("Root of the powers of Earth (Taurus, Virgo, Capricorn)",
                     "Jupiter in Capricorn", "Mars in Capricorn", "Sun in Capricorn", "Mercury in Taurus",
                     "Moon in Taurus", "Saturn in Taurus", "Sun in Virgo", "Venus in Virgo", "Mercury in Virgo",
                     "Earth of Earth", "Fire of Earth - Leo/Virgo", "Water of Earth - Sagittarius/Capricorn",
                     "Air of Earth - Aries/Taurus")

build_suit <- function(suit, meanings, astro) {
  meta <- suit_meta[[suit]]
  stopifnot(nrow(meanings) == 14, length(astro) == 14)
  data.frame(
    name       = paste(rank_names, "of", suit),
    arcana     = "Minor Arcana",
    suit       = suit,
    rank       = rank_names,
    number     = 1:14,
    upright    = meanings[, 1],
    reversed   = meanings[, 2],
    element    = meta$element,
    astrology  = astro,
    numerology = minor_numerology(1:14),
    glyph      = paste0(meta$glyph, "\uFE0E"),
    keywords   = meta$keywords,
    stringsAsFactors = FALSE
  )
}

deck_df <- rbind(
  majors,
  build_suit("Wands", wands_m, wands_astro),
  build_suit("Cups", cups_m, cups_astro),
  build_suit("Swords", swords_m, swords_astro),
  build_suit("Pentacles", pentacles_m, pentacles_astro)
)

major_files <- paste0(sprintf("%02d", 0:21), "_", c(
  "Fool", "Magician", "High_Priestess", "Empress", "Emperor", "Hierophant", "Lovers",
  "Chariot", "Strength", "Hermit", "Wheel_of_Fortune", "Justice", "Hanged_Man",
  "Death", "Temperance", "Devil", "Tower", "Star", "Moon", "Sun", "Judgement", "World"
), ".jpg")

minor_files <- as.vector(outer(sprintf("%02d", 1:14), suits,
                               function(n, s) paste0(s, n, ".jpg")))

deck_df$file <- c(major_files,minor_files)

  
rownames(deck_df) <- NULL
rownames(deck_df) <- seq_len(nrow(deck_df))
stopifnot(nrow(deck_df) == 78)

write.csv(deck_df, "tarot_df.csv")
