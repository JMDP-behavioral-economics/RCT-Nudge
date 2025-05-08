# 主な修正方針

- 異質性の議論をトーンダウンさせて、CTをメインアウトカムにする
- 返信関連のアウトカムはメカニズムの議論で使う
- 学術的な貢献を強調するようにする

# 具体的な修正

**1. Introduction（大幅に改訂）**

主要な修正点：

- 異質性の重要性を議論するところは削除
  - AJHEバーションの"Considering...(p.4)"からの二段落を削除
- 一段落目：死後臓器移植の研究と対比させて、本研究の目的と重要性を強調
- 学術的な意義を詳述（p.6 "This study relates to ..."以降）
    1. 死後臓器移植の政策目標が登録者を増やすことであるのに対して、本研究の目標は登録者の脱落を防ぐこと
    1. 一般的な介入実験と比較して、利他的な人が多くいる集団に対してナッジを仕掛ける。

**2. Field Experiment**

**2.3. Data and Empirical Strategy**

- Secondary outcome（返信関連）の議論を削除
  - AJHEバージョンの"To better understand... (p.13)"の段落を削除
- 制御変数に「休日の日数」と「CT省略対象か否か」を追加
  - 休日の日数：適合通知を受け取った週とその翌週の休日日数（年末年始の休日は12/29~1/3とした）
  - CT省略対象か否か：過去の確認検査を利用できる状態かどうかをデータで識別するので、これを個人属性として扱う。アウトカムではCT省略した場合をCT到達として扱う。
    - CT省略されたケースを除いた分析でも結果は変わらず

**3. Results**

**3.1. Average Treatment Effects on CT**

- この節ではCTへの効果のみを論文本体に示し、返信関連への効果は補論に移動。
  - 返信関連への効果はあくまでCTへの効果のメカニズムの議論に用いた。"As mentioned previously,... (p.16)"を参照。
- 異質性の分析結果は補論に移動し、3.2 Heterogeneous Treatment Effects on CTという節を用意。
  - AJHEバージョンは性別×年齢階級のサブグループ分析を行った。しかし、small sample sizeにより、頑健な結果が得られないため、性別のみの交差項モデルに留めた。
  - AJHEのレフェリーのsuggestionにより、初めて適合通知を受け取ったかどうかの交差項モデルも追加

**3.3. Effects on the Coordination Process After CT**

**4. Conclusions**

- レフェリーのsuggestionにより、実験群Bの効果が「登録者を何人増やす」ことに相当するかという費用対効果に近い議論を追加。"To evaluate the cost effectiveness of the intervention ... (p.23)"の段落を参照。
- 異質性の分析の結果、実験群Bは過去に適合通知を受け取っている男性ドナーによく効いた可能性がある。「移植医師が男性ドナーを好んでいる」という点を踏まえて、この可能性の重要性を議論。"Although we cannot obtain robust evidence of heterogeneous treatment effects ... (p.22)"の段落を参照。
- ドナー側のデータだけでは移植率への効果を正確に測定することは難しいということ伝えて、移植到達率への効果が統計的に非有意であることをディフェンスしている。"Whether the probability message increased ... (p.24)"の段落を参照。
