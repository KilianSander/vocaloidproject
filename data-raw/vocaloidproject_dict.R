## code to prepare `vocaloidproject_dict`
educational_degrees <-
  tibble::tribble(
    ~key, ~de_f, ~ja,
    "EDU_CHOICE1", "Berufsausbildung/Fachhochschule/Fachoberschule", "準学士/高度専門士/専門士",
    "EDU_CHOICE2", "Abitur (HS)", "高卒",
    "EDU_CHOICE3", "Fachhochschuldiplom", "短期大学士",
    "EDU_CHOICE4", "Bachelor (Uni)", "学士",
    "EDU_CHOICE5", "Master", "修士",
    "EDU_CHOICE6", "Promotion/PhD", "博士",
    "EDU_PROMPT", "Was ist Ihr höchster Bildungsabschluss?", "EDU_PROMPT"
  ) |>
  dplyr::mutate(
    de = de_f |> stringr::str_replace("Ihr", "Dein")
  )

student <-
  tibble::tribble(
    ~key, ~de_f, ~de, ~en, ~ja,
    "STUDENT_PROMPT", "Studieren Sie aktuell (z.&nbsp;B. an einer Universität)?",
    "Studierst Du aktuell (z.&nbsp;B. an einer Universität)?",
    "Are you currently studying (e.&nbsp;g. at a university)",
    "現在勉強中ですか（大学など）？", # deepl
    "STUDENT_CHOICE1", "ja", "ja", "yes", "はい",
    "STUDENT_CHOICE2", "nein", "nein", "no", "いいえ"
  )

## get GMS dict from psyquest
gms_dict <-
  psyquest::psyquest_dict$as.data.frame() |>
  dplyr::filter(
    stringr::str_detect(
      key,
      "^TGMS"
    )
  ) |>
  dplyr::select(
    key,
    tidyselect::any_of(c("de", "de_f", "en"))
  ) |>
  dplyr::left_join(
    by = "key",
    y = tibble::tribble(
      ~key, ~ja,
      "TGMS_0000_PROMPT", "ゴールドスミス音楽洗練指標",
      "TGMS_0001_CHOICE1", "全くあてはまらない ",
      "TGMS_0001_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0001_CHOICE3", "あまりあてはまらない",
      "TGMS_0001_CHOICE4", "どちらでもない",
      "TGMS_0001_CHOICE5", "わりとあてはまる",
      "TGMS_0001_CHOICE6", "よくあてはまる",
      "TGMS_0001_CHOICE7", "実によくあてはまる",
      "TGMS_0001_PROMPT", "自由時間のほとんどを、音楽に関することに使う",
      "TGMS_0002_CHOICE1", "0",
      "TGMS_0002_CHOICE2", "1",
      "TGMS_0002_CHOICE3", "2",
      "TGMS_0002_CHOICE4", "3",
      "TGMS_0002_CHOICE5", "4-5",
      "TGMS_0002_CHOICE6", "6-9",
      "TGMS_0002_CHOICE7", "10 かそれ以上",
      "TGMS_0002_PROMPT", "私はここ「？」年間、日常的・定期的に楽器（歌を含む）の練習をしている",
      "TGMS_0003_CHOICE1", "全くあてはまらない",
      "TGMS_0003_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0003_CHOICE3", "あまりあてはまらない",
      "TGMS_0003_CHOICE4", "どちらでもない",
      "TGMS_0003_CHOICE5", "わりとあてはまる",
      "TGMS_0003_CHOICE6", "よくあてはまる",
      "TGMS_0003_CHOICE7", "実によくあてはまる",
      "TGMS_0003_PROMPT", "ゾクゾクするような（もしくは鳥肌が立つような）音楽を聴きたいと思うことがある",
      "TGMS_0004_CHOICE1", "全くあてはまらない",
      "TGMS_0004_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0004_CHOICE3", "あまりあてはまらない",
      "TGMS_0004_CHOICE4", "どちらでもない",
      "TGMS_0004_CHOICE5", "わりとあてはまる",
      "TGMS_0004_CHOICE6", "よくあてはまる",
      "TGMS_0004_CHOICE7", "実によくあてはまる",
      "TGMS_0004_PROMPT", "インターネットのブログや掲示板に音楽に関する情報を書くのが楽しい",
      "TGMS_0005_CHOICE1", "全くあてはまらない",
      "TGMS_0005_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0005_CHOICE3", "あまりあてはまらない",
      "TGMS_0005_CHOICE4", "どちらでもない",
      "TGMS_0005_CHOICE5", "わりとあてはまる",
      "TGMS_0005_CHOICE6", "よくあてはまる",
      "TGMS_0005_CHOICE7", "実によくあてはまる",
      "TGMS_0005_PROMPT", "知らないメロディーを誰かが歌い始めた時、たいていの場合は一緒に歌うことができる",
      "TGMS_0006_CHOICE1", "全くあてはまらない",
      "TGMS_0006_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0006_CHOICE3", "あまりあてはまらない",
      "TGMS_0006_CHOICE4", "どちらでもない",
      "TGMS_0006_CHOICE5", "わりとあてはまる",
      "TGMS_0006_CHOICE6", "よくあてはまる",
      "TGMS_0006_CHOICE7", "実によくあてはまる",
      "TGMS_0006_PROMPT", "歌っている人がうまいかどうか、的確に判断ができる",
      "TGMS_0007_CHOICE1", "全くあてはまらない",
      "TGMS_0007_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0007_CHOICE3", "あまりあてはまらない",
      "TGMS_0007_CHOICE4", "どちらでもない",
      "TGMS_0007_CHOICE5", "わりとあてはまる",
      "TGMS_0007_CHOICE6", "よくあてはまる",
      "TGMS_0007_CHOICE7", "実によくあてはまる",
      "TGMS_0007_PROMPT", "聴こえてきた曲が初めて聴く曲かどうかがすぐにわかる",
      "TGMS_0008_CHOICE1", "全くあてはまらない",
      "TGMS_0008_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0008_CHOICE3", "あまりあてはまらない",
      "TGMS_0008_CHOICE4", "どちらでもない",
      "TGMS_0008_CHOICE5", "わりとあてはまる",
      "TGMS_0008_CHOICE6", "よくあてはまる",
      "TGMS_0008_CHOICE7", "実によくあてはまる",
      "TGMS_0008_PROMPT", "暗譜で演奏することができる（歌を含む）",
      "TGMS_0009_CHOICE1", "全くあてはまらない",
      "TGMS_0009_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0009_CHOICE3", "あまりあてはまらない",
      "TGMS_0009_CHOICE4", "どちらでもない",
      "TGMS_0009_CHOICE5", "わりとあてはまる",
      "TGMS_0009_CHOICE6", "よくあてはまる",
      "TGMS_0009_CHOICE7", "実によくあてはまる",
      "TGMS_0009_PROMPT", "馴染みのない音楽を耳にすると、それについてもっと知りたいと思う",
      "TGMS_0010_CHOICE1", "全くあてはまらない",
      "TGMS_0010_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0010_CHOICE3", "あまりあてはまらない",
      "TGMS_0010_CHOICE4", "どちらでもない",
      "TGMS_0010_CHOICE5", "わりとあてはまる",
      "TGMS_0010_CHOICE6", "よくあてはまる",
      "TGMS_0010_CHOICE7", "実によくあてはまる",
      "TGMS_0010_PROMPT", "音楽を聴いてもほとんど感情が揺さぶられない",
      "TGMS_0011_CHOICE1", "全くあてはまらない",
      "TGMS_0011_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0011_CHOICE3", "あまりあてはまらない",
      "TGMS_0011_CHOICE4", "どちらでもない",
      "TGMS_0011_CHOICE5", "わりとあてはまる",
      "TGMS_0011_CHOICE6", "よくあてはまる",
      "TGMS_0011_CHOICE7", "実によくあてはまる",
      "TGMS_0011_PROMPT", "知っている曲のメロディーにぴったり合わせて歌うことができる",
      "TGMS_0012_CHOICE1", "0",
      "TGMS_0012_CHOICE2", "0.5",
      "TGMS_0012_CHOICE3", "1",
      "TGMS_0012_CHOICE4", "1.5",
      "TGMS_0012_CHOICE5", "2",
      "TGMS_0012_CHOICE6", "3-4",
      "TGMS_0012_CHOICE7", "5 かそれ以上",
      "TGMS_0012_PROMPT", "興味が最も高かった時期には、メインの楽器（歌を含む）を一日に「?」時間練習した",
      "TGMS_0013_CHOICE1", "全くあてはまらない",
      "TGMS_0013_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0013_CHOICE3", "あまりあてはまらない",
      "TGMS_0013_CHOICE4", "どちらでもない",
      "TGMS_0013_CHOICE5", "わりとあてはまる",
      "TGMS_0013_CHOICE6", "よくあてはまる",
      "TGMS_0013_CHOICE7", "実によくあてはまる",
      "TGMS_0013_PROMPT", "知っている曲をアーティストがどこか間違って演奏しても、なかなか気づけない",
      "TGMS_0014_CHOICE1", "全くあてはまらない",
      "TGMS_0014_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0014_CHOICE3", "あまりあてはまらない",
      "TGMS_0014_CHOICE4", "どちらでもない",
      "TGMS_0014_CHOICE5", "わりとあてはまる",
      "TGMS_0014_CHOICE6", "よくあてはまる",
      "TGMS_0014_CHOICE7", "実によくあてはまる",
      "TGMS_0014_PROMPT", "同じ曲の違う演奏を聴いたとき、その違いについて比べたり話すことができる",
      "TGMS_0015_CHOICE1", "全くあてはまらない",
      "TGMS_0015_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0015_CHOICE3", "あまりあてはまらない",
      "TGMS_0015_CHOICE4", "どちらでもない",
      "TGMS_0015_CHOICE5", "わりとあてはまる",
      "TGMS_0015_CHOICE6", "よくあてはまる",
      "TGMS_0015_CHOICE7", "実によくあてはまる",
      "TGMS_0015_PROMPT", "よく知っている曲でも、いつもと違うやり方で演奏されたり別の人が演奏していると、その曲だとわからないことがある",
      "TGMS_0016_CHOICE1", "全くあてはまらない",
      "TGMS_0016_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0016_CHOICE3", "あまりあてはまらない",
      "TGMS_0016_CHOICE4", "どちらでもない",
      "TGMS_0016_CHOICE5", "わりとあてはまる",
      "TGMS_0016_CHOICE6", "よくあてはまる",
      "TGMS_0016_CHOICE7", "実によくあてはまる",
      "TGMS_0016_PROMPT", "自分の音楽的才能や音楽演奏を褒められたことがない",
      "TGMS_0017_CHOICE1", "0",
      "TGMS_0017_CHOICE2", "1",
      "TGMS_0017_CHOICE3", "2",
      "TGMS_0017_CHOICE4", "3",
      "TGMS_0017_CHOICE5", "4-6",
      "TGMS_0017_CHOICE6", "7-10",
      "TGMS_0017_CHOICE7", "11かそれ以上",
      "TGMS_0017_PROMPT", "この１２ヶ月の間に「？」回の生演奏イベントを聴きに行った",
      "TGMS_0018_CHOICE1", "0",
      "TGMS_0018_CHOICE2", "0.5",
      "TGMS_0018_CHOICE3", "1",
      "TGMS_0018_CHOICE4", "2",
      "TGMS_0018_CHOICE5", "3",
      "TGMS_0018_CHOICE6", "4-6",
      "TGMS_0018_CHOICE7", "7かそれ以上",
      "TGMS_0018_PROMPT", "音楽理論の訓練を「？」年間受けた",
      "TGMS_0019_CHOICE1", "全くあてはまらない",
      "TGMS_0019_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0019_CHOICE3", "あまりあてはまらない",
      "TGMS_0019_CHOICE4", "どちらでもない",
      "TGMS_0019_CHOICE5", "わりとあてはまる",
      "TGMS_0019_CHOICE6", "よくあてはまる",
      "TGMS_0019_CHOICE7", "実によくあてはまる",
      "TGMS_0019_PROMPT", "よく、音楽に関連した情報をインターネットで検索したり読んだりする",
      "TGMS_0020_CHOICE1", "全くあてはまらない",
      "TGMS_0020_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0020_CHOICE3", "あまりあてはまらない",
      "TGMS_0020_CHOICE4", "どちらでもない",
      "TGMS_0020_CHOICE5", "わりとあてはまる",
      "TGMS_0020_CHOICE6", "よくあてはまる",
      "TGMS_0020_CHOICE7", "実によくあてはまる",
      "TGMS_0020_PROMPT", "やる気を出すために、特定の音楽をかけることがよくある",
      "TGMS_0021_CHOICE1", "0",
      "TGMS_0021_CHOICE2", "0.5",
      "TGMS_0021_CHOICE3", "1",
      "TGMS_0021_CHOICE4", "2",
      "TGMS_0021_CHOICE5", "3-5",
      "TGMS_0021_CHOICE6", "6-9",
      "TGMS_0021_CHOICE7", "10 かそれ以上",
      "TGMS_0021_PROMPT", "私はここ「？」年間、日常的・定期的に楽器（歌を含む）の練習をしている",
      "TGMS_0022_CHOICE1", "0",
      "TGMS_0022_CHOICE2", "1",
      "TGMS_0022_CHOICE3", "2",
      "TGMS_0022_CHOICE4", "3",
      "TGMS_0022_CHOICE5", "4",
      "TGMS_0022_CHOICE6", "5",
      "TGMS_0022_CHOICE7", "6 かそれ以上",
      "TGMS_0022_PROMPT", "私は「？」種類の楽器を演奏できる",
      "TGMS_0023_CHOICE1", "全くあてはまらない",
      "TGMS_0023_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0023_CHOICE3", "あまりあてはまらない",
      "TGMS_0023_CHOICE4", "どちらでもない",
      "TGMS_0023_CHOICE5", "わりとあてはまる",
      "TGMS_0023_CHOICE6", "よくあてはまる",
      "TGMS_0023_CHOICE7", "実によくあてはまる",
      "TGMS_0023_PROMPT", "誰かが私の知っている曲を歌っていても、ハモるのはとても難しい",
      "TGMS_0024_CHOICE1", "全くあてはまらない",
      "TGMS_0024_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0024_CHOICE3", "あまりあてはまらない",
      "TGMS_0024_CHOICE4", "どちらでもない",
      "TGMS_0024_CHOICE5", "わりとあてはまる",
      "TGMS_0024_CHOICE6", "よくあてはまる",
      "TGMS_0024_CHOICE7", "実によくあてはまる",
      "TGMS_0024_PROMPT", "他の人の歌や演奏が本来のビートからずれていることがわかる",
      "TGMS_0025_CHOICE1", "全くあてはまらない",
      "TGMS_0025_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0025_CHOICE3", "あまりあてはまらない",
      "TGMS_0025_CHOICE4", "どちらでもない",
      "TGMS_0025_CHOICE5", "わりとあてはまる",
      "TGMS_0025_CHOICE6", "よくあてはまる",
      "TGMS_0025_CHOICE7", "実によくあてはまる",
      "TGMS_0025_PROMPT", "聴こえてきた曲の特徴を説明することができる",
      "TGMS_0026_CHOICE1", "全くあてはまらない",
      "TGMS_0026_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0026_CHOICE3", "あまりあてはまらない",
      "TGMS_0026_CHOICE4", "どちらでもない",
      "TGMS_0026_CHOICE5", "わりとあてはまる",
      "TGMS_0026_CHOICE6", "よくあてはまる",
      "TGMS_0026_CHOICE7", "実によくあてはまる",
      "TGMS_0026_PROMPT", "音楽を聴いたときに湧き上がる感情について話すことができる",
      "TGMS_0027_CHOICE1", "全くあてはまらない",
      "TGMS_0027_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0027_CHOICE3", "あまりあてはまらない",
      "TGMS_0027_CHOICE4", "どちらでもない",
      "TGMS_0027_CHOICE5", "わりとあてはまる",
      "TGMS_0027_CHOICE6", "よくあてはまる",
      "TGMS_0027_CHOICE7", "実によくあてはまる",
      "TGMS_0027_PROMPT", "自由に使えるお金を、音楽のためには使わない",
      "TGMS_0028_CHOICE1", "全くあてはまらない",
      "TGMS_0028_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0028_CHOICE3", "あまりあてはまらない",
      "TGMS_0028_CHOICE4", "どちらでもない",
      "TGMS_0028_CHOICE5", "わりとあてはまる",
      "TGMS_0028_CHOICE6", "よくあてはまる",
      "TGMS_0028_CHOICE7", "実によくあてはまる",
      "TGMS_0028_PROMPT", "他の人の歌や、演奏の音が外れているかどうかわかる",
      "TGMS_0029_CHOICE1", "全くあてはまらない",
      "TGMS_0029_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0029_CHOICE3", "あまりあてはまらない",
      "TGMS_0029_CHOICE4", "どちらでもない",
      "TGMS_0029_CHOICE5", "わりとあてはまる",
      "TGMS_0029_CHOICE6", "よくあてはまる",
      "TGMS_0029_CHOICE7", "実によくあてはまる",
      "TGMS_0029_PROMPT", "自分が歌っているときに、音の高さがあっているかどうか自信がない",
      "TGMS_0030_CHOICE1", "全くあてはまらない",
      "TGMS_0030_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0030_CHOICE3", "あまりあてはまらない",
      "TGMS_0030_CHOICE4", "どちらでもない",
      "TGMS_0030_CHOICE5", "わりとあてはまる",
      "TGMS_0030_CHOICE6", "よくあてはまる",
      "TGMS_0030_CHOICE7", "実によくあてはまる",
      "TGMS_0030_PROMPT", "音楽は私にとって一種の中毒で、音楽なしでは生きていけない",
      "TGMS_0031_CHOICE1", "0-15 分",
      "TGMS_0031_CHOICE2", "15-30 分",
      "TGMS_0031_CHOICE3", "30-60 分",
      "TGMS_0031_CHOICE4", "60-90 分",
      "TGMS_0031_CHOICE5", "2 時間",
      "TGMS_0031_CHOICE6", "2-3 時間",
      "TGMS_0031_CHOICE7", "4 時間かそれ以上",
      "TGMS_0031_PROMPT", "一日のうち、集中して音楽を聴く時間の長さはたいてい「？」分（時間）である",
      "TGMS_0032_CHOICE1", "該当しない",
      "TGMS_0032_CHOICE10", "ファゴット",
      "TGMS_0032_CHOICE11", "トランペット",
      "TGMS_0032_CHOICE12", "トロンボーン",
      "TGMS_0032_CHOICE13", "テューバ",
      "TGMS_0032_CHOICE14", "サクソフォン",
      "TGMS_0032_CHOICE15", "ホルン", # CAUTION ! horn
      "TGMS_0032_CHOICE16", "ヴァイオリン",
      "TGMS_0032_CHOICE17", "チェロ",
      "TGMS_0032_CHOICE18", "ヴィオラ",
      "TGMS_0032_CHOICE19", "コントラバス",
      "TGMS_0032_CHOICE2", "声楽",
      "TGMS_0032_CHOICE20", "ハープ",
      "TGMS_0032_CHOICE21", "その他",
      "TGMS_0032_CHOICE3", "ピアノ",
      "TGMS_0032_CHOICE4", "ギター",
      "TGMS_0032_CHOICE5", "ドラム",
      "TGMS_0032_CHOICE6", "シロフォン",　# CAUTION ! xylophone
      "TGMS_0032_CHOICE7", "フルート",
      "TGMS_0032_CHOICE8", "オーボエ",
      "TGMS_0032_CHOICE9", "クラリネット",
      "TGMS_0032_PROMPT", "私がもっとも得意な楽器は「？」である",
      "TGMS_0033_CHOICE1", "全くあてはまらない",
      "TGMS_0033_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0033_CHOICE3", "あまりあてはまらない",
      "TGMS_0033_CHOICE4", "どちらでもない",
      "TGMS_0033_CHOICE5", "わりとあてはまる",
      "TGMS_0033_CHOICE6", "よくあてはまる",
      "TGMS_0033_CHOICE7", "実によくあてはまる",
      "TGMS_0033_PROMPT", "音を外してしまうかもしれないので、人前で歌うのは嫌いだ",
      "TGMS_0034_CHOICE1", "全くあてはまらない",
      "TGMS_0034_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0034_CHOICE3", "あまりあてはまらない",
      "TGMS_0034_CHOICE4", "どちらでもない",
      "TGMS_0034_CHOICE5", "わりとあてはまる",
      "TGMS_0034_CHOICE6", "よくあてはまる",
      "TGMS_0034_CHOICE7", "実によくあてはまる",
      "TGMS_0034_PROMPT", "大抵の場合は、聴いた音楽のジャンルがわかる",
      "TGMS_0035_CHOICE1", "全くあてはまらない",
      "TGMS_0035_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0035_CHOICE3", "あまりあてはまらない",
      "TGMS_0035_CHOICE4", "どちらでもない",
      "TGMS_0035_CHOICE5", "わりとあてはまる",
      "TGMS_0035_CHOICE6", "よくあてはまる",
      "TGMS_0035_CHOICE7", "実によくあてはまる",
      "TGMS_0035_PROMPT", "私は音楽家ではない",
      "TGMS_0036_CHOICE1", "全くあてはまらない",
      "TGMS_0036_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0036_CHOICE3", "あまりあてはまらない",
      "TGMS_0036_CHOICE4", "どちらでもない",
      "TGMS_0036_CHOICE5", "わりとあてはまる",
      "TGMS_0036_CHOICE6", "よくあてはまる",
      "TGMS_0036_CHOICE7", "実によくあてはまる",
      "TGMS_0036_PROMPT", "新しいアーティストや曲の流行を知っている",
      "TGMS_0037_CHOICE1", "全くあてはまらない",
      "TGMS_0037_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0037_CHOICE3", "あまりあてはまらない",
      "TGMS_0037_CHOICE4", "どちらでもない",
      "TGMS_0037_CHOICE5", "わりとあてはまる",
      "TGMS_0037_CHOICE6", "よくあてはまる",
      "TGMS_0037_CHOICE7", "実によくあてはまる",
      "TGMS_0037_PROMPT", "知らない歌でも２、３回聴けば歌うことができる",
      "TGMS_0038_CHOICE1", "全くあてはまらない",
      "TGMS_0038_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0038_CHOICE3", "あまりあてはまらない",
      "TGMS_0038_CHOICE4", "どちらでもない",
      "TGMS_0038_CHOICE5", "わりとあてはまる",
      "TGMS_0038_CHOICE6", "よくあてはまる",
      "TGMS_0038_CHOICE7", "実によくあてはまる",
      "TGMS_0038_PROMPT", "知らない曲でも一回聴けば、数時間あとでも歌うことができる",
      "TGMS_0039_CHOICE1", "全くあてはまらない",
      "TGMS_0039_CHOICE2", "ほとんどあてはまらない",
      "TGMS_0039_CHOICE3", "あまりあてはまらない",
      "TGMS_0039_CHOICE4", "どちらでもない",
      "TGMS_0039_CHOICE5", "わりとあてはまる",
      "TGMS_0039_CHOICE6", "よくあてはまる",
      "TGMS_0039_CHOICE7", "実によくあてはまる",
      "TGMS_0039_PROMPT", "音楽を聴いて、特定の人や場所を思い出すことがある",
      "TGMS_0040_CHOICE1", "2",
      "TGMS_0040_CHOICE10", "11",
      "TGMS_0040_CHOICE11", "12",
      "TGMS_0040_CHOICE12", "13",
      "TGMS_0040_CHOICE13", "14",
      "TGMS_0040_CHOICE14", "15",
      "TGMS_0040_CHOICE15", "16",
      "TGMS_0040_CHOICE16", "17",
      "TGMS_0040_CHOICE17", "18",
      "TGMS_0040_CHOICE18", "19",
      "TGMS_0040_CHOICE19", "該当しない",
      "TGMS_0040_CHOICE2", "3",
      "TGMS_0040_CHOICE3", "4",
      "TGMS_0040_CHOICE4", "5",
      "TGMS_0040_CHOICE5", "6",
      "TGMS_0040_CHOICE6", "7",
      "TGMS_0040_CHOICE7", "8",
      "TGMS_0040_CHOICE8", "9",
      "TGMS_0040_CHOICE9", "10",
      "TGMS_0040_PROMPT", "何歳から楽器を弾き始めましたか？",
      "TGMS_0041_CHOICE1", "はい",
      "TGMS_0041_CHOICE2", "いいえ",
      "TGMS_0041_PROMPT", "あなたには絶対音感がありますか？絶対音感とは、音を聴いた時に他の音と比べなくてもその音の名前がわかる能力です。例えば誰かがピアノで弾いた音がファのシャープだとわかる、など。"
    )
  )

general_dict_raw <-
  readr::read_csv2(
    file = "https://raw.githubusercontent.com/klausfrieler/psyquest/refs/heads/master/data_raw/dicts/general_dict.csv"
  ) |>
  dplyr::select(
    key,
    tidyselect::any_of(c("de", "de_f", "en"))
  ) |>
  dplyr::filter(
    !duplicated(key)
  ) |>
  dplyr::left_join(
    by = "key",
    y = tibble::tribble(
      ~key, ~ja,
      "CONTINUE", "次へ",
      "RESULTS_SAVED", "結果が保存されました。", # deepl
      "CLOSE_BROWSER", "ブラウザウィンドウを閉じることができます。", # deepl
      "PAGE_HEADER", "{{test_length}}問中{{num_question}}問", # deepl
      "JANUARY", "1月", # arabic numerals or Kanji?
      "FEBRUARY", "2月",
      "MARCH", "3月",
      "APRIL", "4月",
      "MAY", "5月",
      "JUNE", "6月",
      "JULY", "7月",
      "AUGUST", "8月",
      "SEPTEMBER", "9月",
      "OCTOBER", "10月",
      "NOVEMBER", "11月",
      "DECEMBER", "12月",
      "NONE", "なし", # deepl
      "YEAR", "年",
      "MONTH", "月",
      "OTHER_NATIONALITY", "その他の国籍", # deepl
      "OTHER_COUNTRY", "その他の国", # deepl
      "OTHER_LANGUAGE", "その他の言語", # deepl
      "CHOOSE_ANSWER", "まず答えを選びなさい！", # deepl based on de
      "CHOOSE_AT_LEAST_ONE_ANSWER", "少なくとも1つの答えを選びなさい！", # deepl based on de
      "ANSWER_NEEDED", "何か入力してください！", #deepl based on de/de_f
      "SELECT_MONTH", "月を選択してください！", # deepl
      "SELECT_YEAR", "年を選択してください！", # deepl
      "E.G.", "例", # deepl
      "SELECT_MONTH_AND_YEAR", "月と年を選択してください！", # deepl
      "ENTER_ID", "IDを入力してください", # deepl
      "ENTER_ID_FORMAL", "IDを入力してください", # deepl
      "PROBLEMS_INFO_1", "問題？", # deepl
      "PROBLEMS_INFO_2", "このページへのリンクを添えて", # deepl based on en
      "PROBLEMS_INFO_3", "までご連絡ください。", # deepl based on en
    )
  )

DEG_dict_raw <-
  readr::read_csv2(
    file = "https://raw.githubusercontent.com/klausfrieler/psyquest/refs/heads/master/data_raw/dicts/DEG_dict.csv"
  ) |>
  dplyr::select(
    key,
    tidyselect::any_of(c("de", "de_f", "en"))
  ) |>
  dplyr::filter(
    stringr::str_detect(
      key,
      pattern = "0000|0004|0008|0010"
    )
  ) |>
  dplyr::left_join(
    by = "key",
    y = tibble::tribble(
      ~key, ~ja,
      "TDEG_0000_PROMPT", "簡単な人口統計学的アンケート", # deepl
      "TDEG_0004_CHOICE1", "女性", # deepl
      "TDEG_0004_CHOICE2", "男性", # deepl
      "TDEG_0004_CHOICE3", "多様", # deepl
      "TDEG_0004_CHOICE4", "言いたくない", # deepl
      "TDEG_0004_PROMPT", "あなたの性別を教えてください。", # deepl
      "TDEG_0008_PROMPT", "母国語は何ですか？", # deepl
      "TDEG_0010_PROMPT", "いつ生まれましたか？" # deepl
    )
  )

info_redirect <-
  tibble::tibble(
    key = c(
      "info_text",
      "return_to_prolific",
      "thanks"
    ),
    de = c(
      "Informationstext", # include html tags
      "Zurück zu Prolific",
      "Vielen Dank für Deine Teilnahme!"
    ),
    de_f = c(
      "Informationstext", # include html tags
      "Zurück zu Prolific",
      "Vielen Dank für Ihre Teilnahme!"
    ),
    en = c(
      "Information on the study",
      "Back to Prolific",
      "Thank you for participating!"
    ),
    ja = c(
      "ja: Informationstext", # include html tags
      "Prolificに戻る", # based on deepl
      "ご参加ありがとうございました" # based on deepl
    )
  )
languages <-
  tibble::tribble(
    ~key, ~de, ~ja, ~en,
    "GERMAN", "Deutsch", "ドイツ語", "German",
    "JAPANESE", "Japanisch", "日本語", "Japanese",
    "ENGLISH", "Englisch", "英語", "English"
  ) |>
  dplyr::mutate(
    de_f = de
  )

emotional_baseline <-
  tibble::tibble(
    key = c(
      paste0("EMOBASE_CHOICE", 1:5),
      "EMOBASE_PROMPT",
      paste0("EMOBASE_ITEM", 1:6)
    ),
    de_f = c(
      "gar nicht", "", "", "", "sehr",
      "Wie sehr haben Sie in den letzten 24 Stunden folgende Emotionen empfunden bzw. gelacht?",
      "Wut",
      "Sorge",
      "Traurigkeit",
      "Glück",
      "Freude",
      "Lachen"
    ),
    de = de_f |> stringr::str_replace("haben Sie", "hast Du"),
    ja = paste0("ja: ", de_f),
    en = NA_character_
  )

# deepl for ja translations in `field`
field <-
  tibble::tribble(
    ~key, ~de_f, ~de, ~en, ~ja,
    "PROFESSIONALFIELDSTUDY_PROMPT",
    "Was ist Ihr Studienfach bzw. Ihr berfuliches Fachgebiet?",
    "Was ist Dein Studienfach bzw. Dein berfuliches Fachgebiet?",
    "What is your field of study or your professional field?",
    "研究分野や好きな科目は何ですか？",
    "PROFESSIONALFIELDSTUDY_CHOICE1", "Psychologie", "Psychologie", "Psychology", "心理学",
    "PROFESSIONALFIELDSTUDY_CHOICE2", "Musik / Musiker*in", "Musik / Musiker*in", "Music / Musician", "音楽、ミュージシャン",
    "PROFESSIONALFIELDSTUDY_CHOICE3", "Musikwissenschaft", "Musikwissenscahft", "Musicology", "音楽学",
    "PROFESSIONALFIELDSTUDY_CHOICE4", "Tontechnik", "Tontechnik", "Sound engineering", "音響工学",
    "PROFESSIONALFIELDSTUDY_CHOICE5", "Andere musikbezogene Fächer", "Andere musikbezogene Fächer", "Other music-related subjects", "その他の音楽関連科目",
    "PROFESSIONALFIELDSTUDY_CHOICE6", "Anderes", "Anderes", "Other", "その他"
  )

vocaloidproject_dict_raw <-
  general_dict_raw |>
  dplyr::bind_rows(
    gms_dict,
    DEG_dict_raw,
    languages,
    educational_degrees,
    student,
    emotional_baseline,
    field,
    info_redirect
  )

vocaloidproject_dict <-
  vocaloidproject_dict_raw |>
  psychTestR::i18n_dict$new()
vocaloidproject_dict_df <-
  vocaloidproject_dict$as.data.frame()

usethis::use_data(
  vocaloidproject_dict,
  overwrite = TRUE,
  internal = FALSE
)

usethis::use_data(
  vocaloidproject_dict_df,
  overwrite = TRUE,
  internal = FALSE
)
