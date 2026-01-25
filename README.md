# ⚽ rdr-team-generator

A **balanced team generator** that takes player skill levels (represented by ⭐ stars) into account to create teams that are **as fair and competitive as possible**, using a **non-deterministic algorithm**.

This means that even with the same set of players, each execution may produce **different team combinations**, while still preserving overall balance and fairness.

---

## 🎯 Project Goal

**rdr-team-generator** was created to solve a common problem in casual and competitive matches: **unfair team draws**.

The goal is to generate teams that are:

- Balanced in overall skill level  
- Fair in total star rating  
- Non-predictable  

All of this while still preserving the element of randomness 🎲

---

## ⭐ How It Works

1. Each player is assigned a skill value (e.g. **1 to 5 stars**).
2. The algorithm:
   - Calculates the total sum of stars
   - Distributes players across teams while **minimizing the skill difference**
3. A controlled randomness factor is applied:
   - Prevents the same input from always producing the same output
   - Keeps teams balanced within an acceptable margin

---

## 🔀 Why Non-Deterministic?

Deterministic algorithms usually result in:

- The same teams every time  
- Repeated combinations  

In **rdr-team-generator**:

- Small variations are intentionally allowed  
- Balance remains the top priority  

This approach avoids predictable results while maintaining fairness.

---

## ▶️ How to Run

```bash
stack build
stack run
