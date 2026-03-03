# better-ranacapa

### Scalable eDNA Visualization Beyond ranacapa  
### 面向大规模 eDNA 数据的可扩展可视化框架

---

## 🌎 English

**better-ranacapa** is an enhanced and scalable visualization framework inspired by ranacapa for exploring large-scale eDNA and metabarcoding datasets.

While ranacapa provides powerful exploratory tools, high-dimensional metadata and fine taxonomic resolution often lead to dense and overwhelming visual outputs.  
better-ranacapa addresses this challenge by introducing structured filtering, adaptive collapsing, and visualization-aware constraints to improve clarity, scalability, and reproducibility.

### Key Features

- Metadata-aware filtering for high-dimensional datasets  
- Prevalence and abundance-based taxa filtering  
- Automatic Top-N collapsing with intelligent defaults  
- Facet pagination to prevent visualization overload  
- Structured separation between computational filtering and display filtering  
- Reproducible parameter export  

### Design Philosophy

Instead of displaying everything at once, better-ranacapa applies staged filtering and adaptive constraints so that plots remain interpretable even as dataset size increases.

The goal is not to generate more figures, but to make figures interpretable at scale.

---

## 🌏 中文

**better-ranacapa** 是一个基于 ranacapa 思想进行改进和扩展的可视化分析框架，专为大规模 eDNA 与宏条形码（metabarcoding）数据设计。

在实际应用中，当 metadata 维度增加或分类层级变细时，原有可视化往往会产生信息密度过高、图像过载的问题。  
better-ranacapa 通过结构化过滤机制、动态层级压缩以及可视化约束机制，提高数据展示的可读性、可扩展性与可复现性。

### 核心特性

- 面向高维 metadata 的智能过滤机制  
- 基于出现频率（prevalence）与丰度（abundance）的 taxa 筛选  
- 自动 Top-N 合并与智能默认参数  
- 分页式 facet 可视化，避免图像爆炸  
- 计算过滤与展示过滤的分层设计  
- 支持参数导出与分析复现  

### 设计理念

better-ranacapa 不再一次性展示全部数据，而是通过分阶段过滤与动态约束机制，使可视化在数据规模扩展时仍然保持可解释性。

目标不是生成更多图，而是让图在大规模数据下依然清晰可读。

---

## 🔬 Use Cases / 使用场景

- eDNA biodiversity exploration  
- Large-scale metabarcoding studies  
- Teaching environments with complex metadata  
- Rapid exploratory analysis before formal statistical modeling  

---

## 🛠 Tech Stack

- R  
- Shiny  
- phyloseq  
- tidyverse  

---

## 📄 License

This project is licensed under the GNU General Public License v3.0 (GPL-3.0).

---

## 🚀 Project Status

This project is under active development.  
Feedback and contributions are welcome.

本项目正在持续开发中，欢迎提出建议与贡献代码。
