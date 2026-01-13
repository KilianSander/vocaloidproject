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
    "EDU_PROMPT", "Was ist Ihr höchster Bildungsabschluss?", "これまでに取得した最高学歴は何ですか" # clc
  ) |>
  dplyr::mutate(
    de = de_f |> stringr::str_replace("Ihr", "Dein")
  )

student <-
  tibble::tribble(
    ~key, ~de_f, ~de, ~en, ~ja,
    "STUDENT_PROMPT", "Studieren Sie aktuell?",
    "Studierst Du aktuell?",
    "Are you currently studying",
    "現在、大学に在学中ですか。", # clc
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
      "RESULTS_SAVED", "結果が保存されています。", # clc
      "CLOSE_BROWSER", "<br>ブラウザのウィンドウを閉じてください。", # clc
      "PAGE_HEADER", "全{{test_length}}問中 {{num_question}}問目", # clc
      "JANUARY", "1月", # Arabic numerals or Kanji? Arabic seems more modern.
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
      "NONE", "無し", # clc
      "YEAR", "年（西暦）", # clc
      "MONTH", "月",
      "OTHER_NATIONALITY", "その他の国籍", # deepl & clc
      "OTHER_COUNTRY", "その他の国", # deepl & clc
      "OTHER_LANGUAGE", "その他の言語", # deepl & clc
      "CHOOSE_ANSWER", "まず、答えを選んでください。", # clc
      "CHOOSE_AT_LEAST_ONE_ANSWER", "少なくとも1つの回答を選択してください。", # clc
      "ANSWER_NEEDED", "何か入力してください！", #deepl based on de/de_f # clc: 回答を選択してください。 for multiple choice
      "SELECT_MONTH", "月を選択してください", # deepl & clc
      "SELECT_YEAR", "年を選択してください", # deepl & clc
      "E.G.", "例", # deepl & clc
      "SELECT_MONTH_AND_YEAR", "年・月を選択してください", # clc
      "ENTER_ID", "IDを入力してください", # deepl & clc
      "ENTER_ID_FORMAL", "IDを入力してください", # deepl & clc
      "PROBLEMS_INFO_1", "", # clc
      "PROBLEMS_INFO_2", "問題がある場合は、", # clc
      "PROBLEMS_INFO_3", "に、このページへのリンクを記載したメールをお送りください。", # clc
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
      "TDEG_0000_PROMPT", "簡単な人口動態調査", # clc
      "TDEG_0004_CHOICE1", "女性", # deepl & clc
      "TDEG_0004_CHOICE2", "男性", # deepl & clc
      "TDEG_0004_CHOICE3", "その他", # deepl
      "TDEG_0004_CHOICE4", "回答しない", # clc
      "TDEG_0004_PROMPT", "あなたの性別を教えてください。", # deepl & clc
      "TDEG_0008_PROMPT", "あなたの母語を教えてください", # clc
      "TDEG_0010_PROMPT", "ご誕生年月を教えてください（西暦）" # clc
    )
  )

info_redirect <-
  tibble::tibble(
    key = c(
      "info_text",
      "return_to_prolific",
      "thanks",
      "technical_error"
    ),
    de = c(
      "Bevor es in dieser Sitzung mit dem Anhören von Musik losgeht, bitten wir Dich, einige Fragen zu beantworten. Es werden Fragen zu Deinen emotionalen und musikalischen Erfahrungen sein. Der Teil wird einige Minuten in Anspruch nehmen.", # include html tags
      "Zurück zu Prolific",
      "Vielen Dank für Deine Teilnahme!",
      "Es ist ein Problem aufgetreten."
    ),
    de_f = c(
      "Bevor es in dieser Sitzung mit dem Anhören von Musik losgeht, bitten wir Sie, einige Fragen zu beantworten. Es werden Fragen zu Ihren emotionalen und musikalischen Erfahrungen sein. Der Teil wird einige Minuten in Anspruch nehmen.", # include html tags
      "Zurück zu Prolific",
      "Vielen Dank für Ihre Teilnahme!",
      "Es ist ein Problem aufgetreten."
    ),
    en = c(
      "Before you start listening to music in this session, we would like to ask you to answer a few questions. These will be questions about your emotional and musical experiences. This part will take a few minutes.",
      "Back to Prolific",
      "Thank you for participating!",
      "An error has occurred."
    ),
    ja = c(
      "このセッションで音楽を聴き始める前に、いくつか質問にお答えください。あなたの感情や音楽に関する経験についての質問です。この部分は数分かかるでしょう。", # include html tags
      "PROLIFIC(プロリッフィック）に戻る", # deepl & clc
      "ご参加ありがとうございました！", # clc
      "何か問題が発生しました。" # clc
    )
  ) |>
  tibble::add_row(
    data.frame(
      key = "info_example_rating",
      de_f = "<p>Es folgt nun der musikalische Teil.</p><p>
        Sie werden 60 kurze Ausschnitte hören und sie beurteilen.
        Dabei interessiert uns, welche Emotionen in der Musik zum Ausdruck gebracht werden,
        z.B. Wut, Freude, Verliebtsein usw.
        Bitte beachten Sie, dass es dabei keine richtige oder falsche Antwort gibt.
        Uns interessiert Ihre Meinung.
        Bitte beachten Sie, dass es einen Unterschied gibt zwischen den Emotionen,
        die durch die Musik zum Ausdruck gebracht werden, und Ihren eigenen Emotionen.
        So kann z.B. ein Song sehr traurig sein, Sie sich dabei aber gut und zufrieden fühlen.
        Wir möchten, dass Sie die Musik bewerten, nicht ihre eigenen Emotionen.
        </p><p>Zuerst ein Beispiel.</p>",
      de = "<p>Es folgt nun der musikalische Teil.</p><p>
        Du wirst 60 kurze Ausschnitte hören und sie beurteilen.
        Dabei interessiert uns, welche Emotionen in der Musik zum Ausdruck gebracht werden,
        z.B. Wut, Freude, Verliebtsein usw.
        Bitte beachte, dass es dabei keine richtige oder falsche Antwort gibt.
        Uns interessiert Deine Meinung.
        Bitte beachte, dass es einen Unterschied gibt zwischen den Emotionen,
        die durch die Musik zum Ausdruck gebracht werden, und Ihren eigenen Emotionen.
        So kann z.B. ein Song sehr traurig sein, Sie sich dabei aber gut und zufrieden fühlen.
        Wir möchten, dass Sie die Musik bewerten, nicht ihre eigenen Emotionen.
        </p><p>Zuerst ein Beispiel.</p>",
      ja = "<p>次は音楽の部分です。</p><p>
        60 の抜粋された短編楽節を聴取し、評価してください。
        特に興味があるのは、楽節で表現されている感情、例えば怒り、喜び、恋などです。
        正解や不正解はないので、ご安心ください。
        知りたいのは、あなたの意見です。
        音楽によって表現されている感情と、ご自身の感情には違いがあることにご留意ください。
        例えば、ある曲は大変悲しい曲であるにもかかわらず、それを聴いて気分が良く、満足感を感じる場合もあります。
        評価していただきたいのは、音楽そのものであり、ご自身の感情ではありません。
        </p><p>まず、例を挙げましょう。</p>",
      en = "info example rating"
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
      paste0("EMOBASE_MATRIX_CHOICE", 1:5),
      "EMOBASE_PROMPT",
      paste0("EMOBASE_ITEM", 1:6)
    ),
    de_f = c(
      "gar nicht", "", "", "", "sehr",
      "Wie sehr haben Sie in den letzten 24 Stunden folgende Emotionen empfunden bzw. gelacht?",
      "**Wut**",
      "**Sorge**",
      "**Traurigkeit**",
      "**Glück**",
      "**Freude**",
      "**Lachen**"
    ),
    de = de_f |> stringr::str_replace("haben Sie", "hast Du"),
    ja = c( # clc
      "無し／まったくない", "", "", "", "非常に",
      "この24時間で、次のような感情をどの程度感じたり笑ったりしましたか",
      "**怒り**",
      "**心配**",
      "**悲しみ**",
      "**幸福**",
      "**喜び**",
      "**笑い**"
    ),
    en = c(
      "Not at all", "", "", "", "very much",
      "How much have you felt the following emotions or laughed in the last 24 hours?",
      "**Anger**", "**Worry**", "**Sadness**", "**Happiness**", "**Joy**", "**Laughing**")
  ) |>
  tibble::add_case(
    key = paste0("EMOBASE_ITEMPROMPT", 1:6),
    de_f = paste0(
      "Wie sehr haben Sie in den letzten 24 Stunden ",
      c(
        paste0(c("**Wut**", "**Sorge**", "**Traurigkeit**", "**Glück**", "**Freude**"), " empfunden?"),
        "**gelacht**?"
      )
    ),
    de = paste0(
      "Wie sehr hast Du in den letzten 24 Stunden ",
      c(
        paste0(c("**Wut**", "**Sorge**", "**Traurigkeit**", "**Glück**", "**Freude**"), " empfunden?"),
        "**gelacht**?"
      )
    ),
    en = paste0(
      "How much ",
      c(
        paste0(c("**anger**", "**worry**", "**sadness**", "**happiness**", "**joy**"), " have you felt"),
        "have you **laughed**"
      ),
      " in the last 24 hours?"
    ),
    ja = paste0( # clc
      "この24時間で、どれくらい",
      c(
        paste0("**", c("怒りを感じ", "心配し", "悲しみを感じ", "幸福を感じ", "喜びを感じ", "笑い"), "**")
      ),
      "ましたか"
    )
  ) |>
  tibble::add_case(
    tibble::tibble(
      key = paste0("EMOBASE_CHOICE", 1:5),
      de = c("1 gar nicht", 2:4, "5 sehr"),
      de_f = de,
      en = c("1 not at all", 2:4, "5 very much"),
      ja = c("1 無し／まったくない", 2:4, "5 非常に") # clc
    )
  ) |>
  tibble::add_case(
    tibble::tibble(
      key = c("picinfo"),
      de_f = "Wir zeigen Ihnen im Folgenden sechs Fotos. Bitte beschreiben Sie anhand der Bild-Skala, wie positiv/negativ und aufgeregt/entspannt das jeweilige Foto ist.",
      de = "Wir zeigen Dir im Folgenden sechs Fotos. Bitte beschreibe  anhand der Bild-Skala, wie positiv/negativ und aufgeregt/entspannt das jeweilige Foto ist.",
      ja = "以下、6枚の写真をお見せします。画像スケールを用いて、それぞれの写真がどれほどポジティブ/ネガティブ、興奮/リラックスしているかを記述してください。"
    )
  )

# deepl for ja translations in `field`
field <-
  tibble::tribble(
    ~key, ~de_f, ~de, ~en, ~ja,
    "PROFESSIONALFIELDSTUDY_PROMPT",
    "Was ist Ihr Studienfach bzw. Ihr berfuliches Fachgebiet?",
    "Was ist Dein Studienfach bzw. Dein berfuliches Fachgebiet?",
    "What is your field of study or your professional field?",
    "専攻・専門分野は何ですか？", # clc
    "PROFESSIONALFIELDSTUDY_CHOICE1", "Psychologie", "Psychologie", "Psychology", "心理学",
    "PROFESSIONALFIELDSTUDY_CHOICE2", "Musik / Musiker*in", "Musik / Musiker*in", "Music / Musician", "音楽・音楽家",
    "PROFESSIONALFIELDSTUDY_CHOICE3", "Musikwissenschaft", "Musikwissenschaft", "Musicology", "音楽学",
    "PROFESSIONALFIELDSTUDY_CHOICE4", "Tontechnik", "Tontechnik", "Sound engineering", "音響工学",
    "PROFESSIONALFIELDSTUDY_CHOICE5", "Andere musikbezogene Fächer", "Andere musikbezogene Fächer", "Other music-related subjects", "他の音楽関係専攻",
    "PROFESSIONALFIELDSTUDY_CHOICE6", "Anderes", "Anderes", "Other", "その他"
  )

voice_rating <-
  tibble::tribble(
    ~key, ~de, ~en, ~ja,
    # deepl
    "VOICERATING_PROMPT", "Wie klingt die Stimme?", "How does the voice sound?", "声はどのように聞こえますか？", # clc
    "VOICERATING_MIN", "künstlich", "artificial", "人工的", # clc
    "VOICERATING_MAX", "menschlich", "human", "人間的" # clc
  ) |>
  dplyr::mutate(
    de_f = de
  )

consent <-
  tibble::tribble(
    ~key, ~de, ~en, ~ja,
    "CONSENT_BUTTON",
    "Ich stimme den Teilnahmebedingungen zu und möchte an der Studie teilnehmen.",
    "I agree to the terms and conditions and would like to participate in the study.",
    "参加条件に同意し、本研究に参加を希望します。", # deepl
    "WELCOME", "Willkommen zu unserer Studie!", "Welcome to our study!", "当調査へようこそ！", # clc
    "SESSION1OF3", "Dies ist Sitzung 1 von 3.", "This is Session 1 of 3.", "これは全3回中の第1回です。", # clc
    "SESSION2OF3", "Dies ist Sitzung 2 von 3.", "This is Session 2 of 3.", "これは全3回中の第2回です。", # clc
    "SESSION3OF3", "Dies ist Sitzung 3 von 3.", "This is Session 3 of 3.", "これは全3回中の第3回です。", # clc
    "SESSION1OF2", "Dies ist Sitzung 1 von 2.", "This is Session 1 of 2.", "これは全2回中の第1回です。", # clc
    "SESSION2OF2", "Dies ist Sitzung 2 von 2.", "This is Session 2 of 2.", "これは全2回中の第２回です。" # clc
  ) |>
  dplyr::mutate(
    de_f = de
  ) |>
  tibble::add_row(
    tibble::tribble(
      ~key, ~de, ~de_f, ~en, ~ja,
      "consent_give",
      "Ich stimme den Teilnahmebedingungen zu und möchte an der Studie teilnehmen.",
      "Ich stimme den Teilnahmebedingungen zu und möchte an der Studie teilnehmen.",
      "I agree to the terms and conditions and would like to participate in the study.",
      "参加条件に同意し、研究への参加を希望します。", #clc
      "consent_no",
      "Ich stimme nicht zu.",
      "Ich stimme nicht zu.",
      "I do not agree to the terms and conditions.",
      "参加条件に同意しません。", #clc
      "consent_text_international1",
      paste0(
        "<p>Vielen Dank, dass Du Dich bereit erklärt hast, an dieser Studie teilzunehmen.</p>",
        "<p>Mit dieser Studie möchten wir den emotionalen Ausdruck japanischer Popmusik untersuchen. Hierfür wirst Du <b>zwei Sitzungen</b>  absolvieren, die <b>jeweils nicht länger als 30&ndash;60 min</b> dauern sollten. ",
        "Jede Sitzung beginnt mit Fragen zu Ihrer Person und Ihren musikalischen Erfahrungen. Anschließend folgt die Bewertung von 60 Musikausschnitten. Dabei gibt es keine richtigen oder falschen Antworten. ",
        "Es ist wichtig, dass Du alle Musikausschnitte aufmerksam anhörst. ",
        "Solltest Du das nicht machen, wird das Experiment abgebrochen und Du wirst zu Prolific zurückgeleitet und von der Studie ausgeschlossen (<q>rejection</q>).</p>",
        "<p>Bitte stelle sicher, dass zwischen Sitzung 2 und der vorherigen Sitzung mindestens vier Stunden liegen (sie kann an einem anderen Tag stattfinden). ",
        "Die Bezahlung erfolgt, nachdem Du beide Sitzungen erfolgreich abgeschlossen hast.",
        "Wir rechnen jeweils mit einer Bearbeitungszeit von ca. einer Woche, um alle Ergebnisse fair auswerten zu können.</p>",
        "<p>Deine Teilnahme ist freiwillig. Voraussetzung zur Teilnahme ist, dass Du <b>18 Jahre oder älter bist, Deutsch Deine Muttersprache ist und Du KEINE Kenntnisse der Japanischen Sprache hast</b>. ",
        "Du kannst Deine Teilnahme jederzeit beenden ohne Angabe von Gründen oder negative Konsequenzen. Bei vorzeitigem Abbruch werden Deine Daten gelöscht.</p>",
        "<p>Alle Informationen, die wir durch die Befragung von Dir erhalten, werden vertraulich behandelt und sind anonymisiert, d.h. können in keiner Weise zu Deiner Person zurückverfolgt werden. ",
        "Die Erhebung und Verarbeitung Deiner Daten erfolgt ohne Angabe Deines Namens. ",
        "Die anonymisierten Daten dienen der Grundlagenforschung und werden sowohl in Fachzeitschriften oder Konferenzpräsentationen veröffentlicht als auch in öffentlich zugänglichen Datenbanken, um <q>open science</q>, d.h. die Nachvollziehbarkeit wissenschaftlicher Forschung zu unterstützen.</p>",
        "<p><p>Bei Fragen oder Anregungen kontaktiere bitte die Leiterin der Studie Dr. Elke Lange, <a href='mailto:elke.lange@ae.mpg.de'>elke.lange@ae.mpg.de</a>, Senior Researcher, Max-Planck-Institut für Empirische Ästhetik, Frankfurt am Main.</p>"
      ),
      paste0(
        "<p>Vielen Dank, dass Sie sich bereit erklärt haben, an dieser Studie teilzunehmen.</p>",
        "<p>Mit dieser Studie möchten wir den emotionalen Ausdruck japanischer Popmusik untersuchen. Hierfür werden Sie <b>zwei Sitzungen</b>  absolvieren, die <b>jeweils nicht länger als 30&ndash;60 min</b>  dauern sollten. ",
        "Jede Sitzung beginnt mit Fragen zu Ihrer Person und Ihren musikalischen Erfahrungen. Anschließend folgt die Bewertung von 60 Musikausschnitten. Dabei gibt es keine richtigen oder falschen Antworten. ",
        "Es ist wichtig, dass Sie alle Musikausschnitte aufmerksam anhören. ",
        "Sollten Sie das nicht machen, wird das Experiment abgebrochen und Sie werden zu Prolific zurückgeleitet und von der Studie ausgeschlossen (<q>rejection</q>).</p>",
        "<p>Bitte stellen Sie sicher, dass zwischen Sitzung 2 und der vorherigen Sitzung mindestens vier Stunden liegen (sie kann an einem anderen Tag stattfinden). ",
        "Die Bezahlung erfolgt, nachdem Sie beide Sitzungen erfolgreich abgeschlossen haben.</p>",
        "Wir rechnen jeweils mit einer Bearbeitungszeit von ca. einer Woche, um alle Ergebnisse fair auswerten zu können.</p>",
        "<p>Ihre Teilnahme ist freiwillig. Voraussetzung zur Teilnahme ist, dass Sie <b>18 Jahre oder älter sind, Deutsch Ihre Muttersprache ist und Sie KEINE Kenntnisse der Japanischen Sprache haben</b>. ",
        "Sie können Ihre Teilnahme jederzeit beenden ohne Angabe von Gründen oder negative Konsequenzen. Bei vorzeitigem Abbruch werden Ihre Daten gelöscht.</p>",
        "<p>Alle Informationen, die wir durch die Befragung von Ihnen erhalten, werden vertraulich behandelt und sind anonymisiert, d.h. können in keiner Weise zu Ihrer Person zurückverfolgt werden. ",
        "Die Erhebung und Verarbeitung Ihrer Daten erfolgt ohne Angabe Ihres Namens. ",
        "Die anonymisierten Daten dienen der Grundlagenforschung und werden sowohl in Fachzeitschriften oder Konferenzpräsentationen veröffentlicht als auch in öffentlich zugänglichen Datenbanken, um <q>open science</q>, d.h. die Nachvollziehbarkeit wissenschaftlicher Forschung zu unterstützen.</p>",
        "<p><p>Bei Fragen oder Anregungen kontaktieren Sie bitte die Leiterin der Studie Dr. Elke Lange, <a href='mailto:elke.lange@ae.mpg.de'>elke.lange@ae.mpg.de</a>, Senior Researcher, Max-Planck-Institut für Empirische Ästhetik, Frankfurt am Main.</p>"
      ),
      paste0(
        "<p>Thank you very much for agreeing to participate in this study.</p>",
        "<p>The aim of this study is to examine the emotional expression of Japanese pop music. You will complete <b>two sessions</b>, each of which should take <b>no longer than 30&ndash;60 minutes<b>. ",
        "Each session will begin with questions about yourself and your musical experiences. This will be followed by the evaluation of 60 music excerpts. There are no right or wrong answers.",
        "It is important that you listen carefully to all music excerpts. ",
        "If you do not do so, the experiment will be terminated and you will be redirected back to Prolific and you will be redirected back to Prolific and excluded form the study (<q>rejection</q>).</p>",
        "<p>Please make sure that at least four hours have passed between Session 2 and the previous session (it can take place on another day).",
        "Payment will be made after you have successfully completed both sessions.",
        "We expect a processing time of approximately one week in order to be able to evaluate all results fairly.</p>",
        "<p>Your participation is voluntary. To participate, you must be <b>18 years of age or older, and have Japanese as your native language</b>. ",
        "You may end your participation at any time without giving any reasons and without any negative consequences. If you withdraw early, your data will be deleted.</p>",
        "<p>All information we obtain from the survey will be treated confidentially and is anonymized, meaning it cannot be traced back to you in any way. ",
        "Your data will be collected and processed without your name or any identifying details. ",
        "The anonymized data will be used for basic research and may be published in academic journals or presented at scientific conferences. It will also be made available in open-access databases to support <q>open science</q>, i.e., the transparency and reproducibility of scientific research.</p>",
        "<p>If you have any questions or comments, please contact the study director, Dr. Cathy Cox, <a href='mailto:cox.cathy@kunitachi.ac.jp'>cox.cathy@kunitachi.ac.jp</a>.</p>"
      ),
      paste0(
        "<p>本研究への参加をご承諾いただき、誠にありがとうございます。</p>",
        "<p>本研究では、日本のポップ音楽における感情表現を調査することを目的としています。そのため、<b>2つのセッション</b>にご参加いただきます。各セッションの所要時間は<b>30〜60分</b>程度です。",
        "各セッションは、参加者ご自身に関する質問と音楽経験についての質問から始まります。その後、60個の音楽サンプルを評価していただきます。これらには正解や不正解はありません。",
        "すべての音楽サンプルを注意深くお聞きいただくことが重要です。",
        "規定に沿ってご対応いただけない場合、実験は中止となり、PROLIFIC(プロリッフィック）へ自動的に戻されます。</p>", # update!
        "<p>セッション2は、少なくとも4時間の間隔を空けて実施してください（別の日でも問題ありません）。",
        "謝礼は、セッション2の完了後にお渡しいたします。", # update!
        "</p>", # update!
        "ご参加は任意です。参加条件は、<b>18歳以上で、母語が日本語であること</b>です。",
        "ご参加はいつでも理由を問わず中止することができ、何ら不利益はありません。途中で中止された場合、提供いただいたデータは削除されます。</p>",
        "<p>アンケートで取得したすべての情報は機密として扱われ、匿名化されます。つまり、個人が特定されることは一切ありません。",
        "データの収集および処理は、名前や個人を特定できる情報なしで行われます。",
        "匿名化されたデータは基礎研究に利用され、学術誌への掲載や学会での発表に使用されるほか、<q>オープンサイエンス</q>、すなわち科学研究の透明性と再現性を支援するために、公開データベースでも利用可能とされます。</p>",
        "<p>質問やご意見がある場合は、研究責任者 コックス氏, <a href='mailto:cox.cathy@kunitachi.ac.jp'>cox.cathy@kunitachi.ac.jp</a> までご連絡ください。</p>"
      )
    )
  ) |>
  tibble::add_case(
    tibble::tibble(
      key = "consent_text_experiment4",
      de = paste0(
        "<p>Vielen Dank, dass Du Dich bereit erklärt hast, an dieser Studie teilzunehmen.</p>",
        "<p>Mit dieser Studie möchten wir den emotionalen Ausdruck japanischer Popmusik untersuchen. Hierfür wirst Du <b>drei Sitzungen</b>  absolvieren, die <b>jeweils nicht länger als 30&ndash;60 min</b> dauern sollten. ",
        "Jede Sitzung beginnt mit Fragen zu Ihrer Person und Ihren musikalischen Erfahrungen. Anschließend folgt die Bewertung von 40 Musikausschnitten. Dabei gibt es keine richtigen oder falschen Antworten. ",
        "Es ist wichtig, dass Du alle Musikausschnitte aufmerksam anhörst. ",
        "Solltest Du das nicht machen, wird das Experiment abgebrochen und Du wirst zu Prolific zurückgeleitet und von der Studie ausgeschlossen (<q>rejection</q>).</p>",
        "<p>",
        "Die Bezahlung erfolgt, nachdem Du alle drei Sitzungen erfolgreich abgeschlossen hast.",
        "Wir rechnen jeweils mit einer Bearbeitungszeit von ca. einer Woche, um alle Ergebnisse fair auswerten zu können.</p>",
        "<p>Deine Teilnahme ist freiwillig. Voraussetzung zur Teilnahme ist, dass Du <b>18 Jahre oder älter bist, Deutsch Deine Muttersprache ist und Du KEINE Kenntnisse der Japanischen Sprache hast</b>. ",
        "Du kannst Deine Teilnahme jederzeit beenden ohne Angabe von Gründen oder negative Konsequenzen. Bei vorzeitigem Abbruch werden Deine Daten gelöscht.</p>",
        "<p>Alle Informationen, die wir durch die Befragung von Dir erhalten, werden vertraulich behandelt und sind anonymisiert, d.h. können in keiner Weise zu Deiner Person zurückverfolgt werden. ",
        "Die Erhebung und Verarbeitung Deiner Daten erfolgt ohne Angabe Deines Namens. ",
        "Die anonymisierten Daten dienen der Grundlagenforschung und werden sowohl in Fachzeitschriften oder Konferenzpräsentationen veröffentlicht als auch in öffentlich zugänglichen Datenbanken, um <q>open science</q>, d.h. die Nachvollziehbarkeit wissenschaftlicher Forschung zu unterstützen.</p>",
        "<p><p>Bei Fragen oder Anregungen kontaktiere bitte die Leiterin der Studie Dr. Elke Lange, <a href='mailto:elke.lange@ae.mpg.de'>elke.lange@ae.mpg.de</a>, Senior Researcher, Max-Planck-Institut für Empirische Ästhetik, Frankfurt am Main.</p>"
      ),
      de_f = paste0(
        "<p>Vielen Dank, dass Sie sich bereit erklärt haben, an dieser Studie teilzunehmen.</p>",
        "<p>Mit dieser Studie möchten wir den emotionalen Ausdruck japanischer Popmusik untersuchen. Hierfür werden Sie <b>drei Sitzungen</b>  absolvieren, die <b>jeweils nicht länger als 30&ndash;60 min</b>  dauern sollten. ",
        "Jede Sitzung beginnt mit Fragen zu Ihrer Person und Ihren musikalischen Erfahrungen. Anschließend folgt die Bewertung von 40 Musikausschnitten. Dabei gibt es keine richtigen oder falschen Antworten. ",
        "Es ist wichtig, dass Sie alle Musikausschnitte aufmerksam anhören. ",
        "Sollten Sie das nicht machen, wird das Experiment abgebrochen und Sie werden zu Prolific zurückgeleitet und von der Studie ausgeschlossen (<q>rejection</q>).</p>",
        "<p> ",
        "Die Bezahlung erfolgt, nachdem Sie alle drei Sitzungen erfolgreich abgeschlossen haben.</p>",
        "Wir rechnen jeweils mit einer Bearbeitungszeit von ca. einer Woche, um alle Ergebnisse fair auswerten zu können.</p>",
        "<p>Ihre Teilnahme ist freiwillig. Voraussetzung zur Teilnahme ist, dass Sie <b>18 Jahre oder älter sind, Deutsch Ihre Muttersprache ist und Sie KEINE Kenntnisse der Japanischen Sprache haben</b>. ",
        "Sie können Ihre Teilnahme jederzeit beenden ohne Angabe von Gründen oder negative Konsequenzen. Bei vorzeitigem Abbruch werden Ihre Daten gelöscht.</p>",
        "<p>Alle Informationen, die wir durch die Befragung von Ihnen erhalten, werden vertraulich behandelt und sind anonymisiert, d.h. können in keiner Weise zu Ihrer Person zurückverfolgt werden. ",
        "Die Erhebung und Verarbeitung Ihrer Daten erfolgt ohne Angabe Ihres Namens. ",
        "Die anonymisierten Daten dienen der Grundlagenforschung und werden sowohl in Fachzeitschriften oder Konferenzpräsentationen veröffentlicht als auch in öffentlich zugänglichen Datenbanken, um <q>open science</q>, d.h. die Nachvollziehbarkeit wissenschaftlicher Forschung zu unterstützen.</p>",
        "<p><p>Bei Fragen oder Anregungen kontaktieren Sie bitte die Leiterin der Studie Dr. Elke Lange, <a href='mailto:elke.lange@ae.mpg.de'>elke.lange@ae.mpg.de</a>, Senior Researcher, Max-Planck-Institut für Empirische Ästhetik, Frankfurt am Main.</p>"
      ),
      en = paste0(
        "<p>Thank you very much for agreeing to participate in this study.</p>",
        "<p>The aim of this study is to examine the emotional expression of Japanese pop music. You will complete <b>three sessions</b>, each of which should take <b>no longer than 30&ndash;60 minutes<b>. ",
        "Each session will begin with questions about yourself and your musical experiences. This will be followed by the evaluation of 40 music excerpts. There are no right or wrong answers.",
        "It is important that you listen carefully to all music excerpts. ",
        "If you do not do so, the experiment will be terminated and you will be redirected back to Prolific and you will be redirected back to Prolific and excluded form the study (<q>rejection</q>).</p>",
        "<p>",
        "Payment will be made after you have successfully completed all three sessions.",
        "We expect a processing time of approximately one week in order to be able to evaluate all results fairly.</p>",
        "<p>Your participation is voluntary. To participate, you must be <b>18 years of age or older, and have Japanese as your native language</b>. ",
        "You may end your participation at any time without giving any reasons and without any negative consequences. If you withdraw early, your data will be deleted.</p>",
        "<p>All information we obtain from the survey will be treated confidentially and is anonymized, meaning it cannot be traced back to you in any way. ",
        "Your data will be collected and processed without your name or any identifying details. ",
        "The anonymized data will be used for basic research and may be published in academic journals or presented at scientific conferences. It will also be made available in open-access databases to support <q>open science</q>, i.e., the transparency and reproducibility of scientific research.</p>",
        "<p>If you have any questions or comments, please contact the study director, Dr. Cathy Cox, <a href='mailto:cox.cathy@kunitachi.ac.jp'>cox.cathy@kunitachi.ac.jp</a>.</p>"
      ),
      ja = paste0(
        "<p>本研究への参加をご承諾いただき、誠にありがとうございます。</p>",
        "<p>本研究では、日本のポップ音楽における感情表現を調査することを目的としています。そのため、<b>3つのセッション</b>にご参加いただきます。各セッションの所要時間は<b>30〜60分</b>程度です。",
        "各セッションは、参加者ご自身に関する質問と音楽経験についての質問から始まります。その後、40個の音楽サンプルを評価していただきます。これらには正解や不正解はありません。",
        "すべての音楽サンプルを注意深くお聞きいただくことが重要です。",
        "規定に沿ってご対応いただけない場合、実験は中止となり、PROLIFIC(プロリッフィック）へ自動的に戻されます。</p>", # update!
        "<p>",
        "謝礼は、セッション3の完了後にお渡しいたします。", # update!
        "</p>", # update!
        "ご参加は任意です。参加条件は、<b>18歳以上で、母語が日本語であること</b>です。",
        "ご参加はいつでも理由を問わず中止することができ、何ら不利益はありません。途中で中止された場合、提供いただいたデータは削除されます。</p>",
        "<p>アンケートで取得したすべての情報は機密として扱われ、匿名化されます。つまり、個人が特定されることは一切ありません。",
        "データの収集および処理は、名前や個人を特定できる情報なしで行われます。",
        "匿名化されたデータは基礎研究に利用され、学術誌への掲載や学会での発表に使用されるほか、<q>オープンサイエンス</q>、すなわち科学研究の透明性と再現性を支援するために、公開データベースでも利用可能とされます。</p>",
        "<p>質問やご意見がある場合は、研究責任者 コックス氏, <a href='mailto:cox.cathy@kunitachi.ac.jp'>cox.cathy@kunitachi.ac.jp</a> までご連絡ください。</p>"
      )
    )
  ) |>
  tibble::add_case(
      key = "consent_not_given_international1",
      en = "As you have indicated that you do not consent to participate in this study please return this submission on Prolific by selecting the <q>stop without completing</q> button.",
      de_f = "Da Sie angegeben haben, der Teilnahme an dieser Studie nicht zuzustimmen, ziehen Sie bitte Ihre Teilnahme auf Prolific zurück durch den Button <q>stop without completing</q>.",
      de = "Da Du angegeben hast, der Teilnahme an dieser Studie nicht zuzustimmen, ziehe bitte Deine Teilnahme auf Prolific zurück durch den Button <q>stop without completing</q>.",
      ja = "ja <q>stop without completing</q>"
  ) |>
  tibble::add_case(
    key = "prolific_return_submission",
    en = "Please return this submission on Prolific by selecting the <q>stop without completing</q> button.",
    de = "Bitte ziehe Deine Teilnahme auf Prolific zurück durch den Button <q>stop without completing</q>.",
    de_f = "Bitte ziehen Sie Ihre Teilnahme auf Prolific zurück durch den Button <q>stop without completing</q>.",
    ja = "ja PROLIFIC(プロリッフィック）<q>stop without completing</q>"
  )

japanese_skills <-
  tibble::tribble(
    ~key, ~de_f,
    "JAPANESE_SKILLS_PROMPT", "Haben Sie Kenntnisse der Japanischen Sprache?",
    "JAPANESE_SKILLS_CHOICE1", "nein",
    "JAPANESE_SKILLS_CHOICE2", "kaum",
    "JAPANESE_SKILLS_CHOICE3", "einige",
    "JAPANESE_SKILLS_CHOICE4", "solide"
  ) |>
  dplyr::mutate(
    de = de_f |> stringr::str_replace_all("Haben Sie", "Hast Du"),
    ja = c( # clc
      "日本語の知識はありますか／日本語できますか",
      "いいえ",
      "あまりない",
      "いくつか",
      "心底"
    )
  ) |>
  tibble::add_case(
    key = "JAPANESE_SKILLS_NOT_MATCHING",
    de = "Du hast Japanischkenntnise angegeben, die nicht zu den Voraussetzungen dieser Studie passen.",
    de_f = "Sie haben Japanischkenntnise angegeben, die nicht zu den Voraussetzungen dieser Studie passen.",
    ja = "ja japanese skills do not match study requirements"
  )

volume_calibration <-
  tibble::tibble(
    key = "volume_calibration_prompt",
    de = "Du solltest eine Tonwiedergabe hören. Bitte stelle die Lautstärke auf ein angenehmes Niveau ein, bevor Sie fortfahren.",
    de_f = "Sie sollten eine Tonwiedergabe hören. Bitte stellen Sie die Lautstärke auf ein angenehmes Niveau ein, bevor Sie fortfahren.",
    en = "You should hear some audio playing. Please adjust the volume to a comfortable level before continuing.",
    ja = "音の再生が聞こえるはずです。続行する前に、音量を快適なレベルに調整してください。" # clc
  )

HALT_dict <-
  HALT::HALT_dict$as.data.frame() |>
  dplyr::filter(
    !(key %in% c(
      "CONTINUE",
      "ENTER_ID",
      paste0("PROBLEMS_INFO_", 1:3),
      "WELCOME"
    ))
  )
HALT_dict <-
  HALT_dict |>
  dplyr::left_join(
    tibble::tribble(
      ~key, ~ja,
  #     "AGAIN", "",
  #     "DEVICE_CHOICE1", "",
  #     "DEVICE_CHOICE2", "",
  #     "DEVICE_CHOICE3", "",
  #     "DEVICE_CHOICE4", "",
  #     "DEVICE_CHOICE5", "",
  #     "DEVICE_CHOICE6", "",
  #     "DEVICE_PROMPT", "",
  #     "INTRO_TEXT", "",
  #     "PAGE_COUNTER", "",
  #     "SCC_PROMPT_HP", "",
  #     "SCC_PROMPT_LS", "",
      "STOP_HEAD", "参加者 様",
      "STOP_TEXT", "アンケートは終了しました。終了の理由は以下のいずれかです：<ol><li>同様の再生デバイスを持つ参加者の数はすでに十分であること。</li><li>ご利用の再生機器は、現在の調査対象に含まれていないこと。</li></ol>今後の調査では、より多くの被験者や他の対象グループを求められるようになる可能性があるため、次回の調査でもまたご参加いただければ幸いです。</p><p>ご協力とご参加、誠にありがとうございました。</p>",
  #     "TESTNAME", "",
  #     "TESTNAME_SHORT", "",
      "THLT_0001_CHOICES", "音量を調整しました", # clc
  #     "THLT_0001_PROMPT", "",
      "THLT_0002_PROMPT", "<h4>音は何回聞こえますか？</h4><p>再生ボタンをクリックすると、短いオーディオサンプルが再生されます。このサンプルでは、音量が大小いくつの音が含まれています。<strong>音量に関わらず、すべての音を数え</strong>、その数値をテキストボックスに入力してください。</p>", # clc
      # "THLT_0003_PROMPT", "",
  #     "THLT_0004_CHOICES1", "",
  #     "THLT_0004_CHOICES2", "",
  #     "THLT_0004_PROMPT", "",
  #     "THLT_0005_PROMPT", "",
  #     "THLT_0006_CHOICES1", "",
  #     "THLT_0006_CHOICES2", "",
  #     "THLT_0006_CHOICES3", "",
  #     "THLT_0006_CHOICES4", "",
  #     "THLT_0006_PROMPT", "",
  #     "THLT_0007_CHOICES1", "",
  #     "THLT_0007_CHOICES2", "",
  #     "THLT_0007_CHOICES3", "",
  #     "THLT_0007_CHOICES4", "",
  #     "THLT_0007_PROMPT", "",
  #     "THLT_0008_PROMPT", "",
  #     "THLT_0009_PROMPT", "",
  #     "THLT_0010_PROMPT", "",
  #     "THLT_0011_PROMPT", "",
  #     "THLT_0012_PROMPT", "",
  #     "THLT_0013_CHOICES1", "",
  #     "THLT_0013_CHOICES2", "",
  #     "THLT_0013_CHOICES3", "",
  #     "THLT_0013_PROMPT", "",
      "WARNING_IMPRECISE", "入力が間違っています。再度お試しください。その際は、より注意深くお聞きください。", # clc
      "WARNING_INCORRECT", "入力が間違っています。再度お試しください。", # clc
      "WARNING_TOO_QUIET", "残念ながら、入力が正しくないようです。再生音量が低すぎる可能性があります。音量をほんの少しだけ上げてから、もう一度お試しください。" # clc
    )
  )

rosas <-
  read.csv(
    file = "data-raw/ROSAS_dict.csv", header = TRUE
  )

asa <-
  read.csv(
    file = "data-raw/ASA_dict.csv", header = TRUE
  )
bes <-
  read.csv(
    file = "data-raw/BES_dict.csv", header = TRUE
  )

landscape <-
  tibble::tribble(
    ~key, ~de_f, ~de, ~en, ~ja,
    "LANDSCAPE_PROMPT",
    "Wenn Sie mit einem Mobilgerät teilnehmen, benutzen Sie dieses bitte im Querformat.",
    "Wenn Du mit einem Mobilgerät teilnimmst, benutze dieses bitte im Querformat.",
    "If you are participating with a mobile device, please use it in landscape mode.",
    "スマホやタブレットなどで参加する場合、画面を横向きに回転してください。" # clc
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
    voice_rating,
    consent,
    japanese_skills,
    volume_calibration,
    HALT_dict,
    rosas,
    asa,
    bes,
    landscape,
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
