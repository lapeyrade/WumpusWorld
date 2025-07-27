# Prolog Approaches for Wumpus World Agent

This document explains the different Prolog-based reasoning approaches implemented for the agents in this Wumpus World simulation. Each approach is contained in its own directory and represents a different trade-off between expressivity, performance, and complexity.

To run any of these examples, navigate to the specific directory and use SWI-Prolog:
```bash
cd <directory_name>
swipl main.pl
```

---

## 1. Default Approach (`prolog_default`)

[Go to `prolog_default` directory](./prolog_default/)

This is the main and recommended approach. It represents a carefully considered compromise between expressivity and efficiency. It uses a Description Logic (DL) like syntax implemented with standard Prolog rules and tabling for performance.

**Key characteristics:**
- **Balanced:** A good mix of expressive power and query speed.
- **Entry Point:** [`main.pl`](./prolog_default/main.pl)

---

## 2. Meta-programming Approach (`prolog_meta`)

[Go to `prolog_meta` directory](./prolog_meta/)

This approach is the most expressive of the four. It heavily uses Prolog's meta-programming capabilities to build a more abstract and powerful ontological reasoning system. While it allows for very rich concept definitions and relationships, it is currently less efficient than the default approach.

**Key characteristics:**
- **Highly Expressive:** Allows for complex and abstract reasoning.
- **Performance:** Less efficient and is a candidate for future optimization.
- **Entry Point:** [`main.pl`](./prolog_meta/main.pl)

---

## 3. SubClassOf Approach (`prolog_subclassof`)

[Go to `prolog_subclassof` directory](./prolog_subclassof/)

This is an earlier and simpler version. Its reasoning is based on a `subClassOf/2` predicate that defines a hierarchy of concepts, similar to class inheritance in object-oriented programming.

**Key characteristics:**
- **Simple:** Easy to understand concept hierarchy.
- **Less Expressive:** Limited to simple inheritance-style relationships.
- **Entry Point:** [`main.pl`](./prolog_subclassof/main.pl)

---

## 4. Query Ontology Approach (`prolog_queryontology`)

[Go to `prolog_queryontology` directory](./prolog_queryontology/)

This is another early and less expressive implementation. It is slightly more advanced than the `subclassof` approach. The core of its reasoning is the `query_ontology/3` predicate, which is used to query the concept hierarchy.

**Key characteristics:**
- **Predicate-based:** Relies on the `query_ontology` predicate for reasoning.
- **Moderately Expressive:** A step up from simple inheritance but still limited compared to the `default` and `meta` approaches.
- **Entry Point:** [`main.pl`](./prolog_queryontology/main.pl) 


---

## Detailed Comparison: `prolog_default` vs. `prolog_meta`

`prolog_default` is not a simplified version of `prolog_meta` in terms of code, but it represents a radical simplification of the **reasoning strategy**. The most precise way to describe the difference is:

*   **`prolog_meta`** uses an **"Ahead-of-Time" (AOT) classification** strategy.
*   **`prolog_default`** uses a **"Query-Time" traversal** strategy.

Let's break down what that means.

### The `prolog_meta` Strategy: Analyze Everything First (AOT)

This approach works like a compiler. Its philosophy is to **do the hard work once, up front.**

1.  **Analysis Phase:** When it starts, it actively reads its own source code using meta-programming. The `classification/2` predicate in [`onto.pl`](./prolog_meta/onto.pl) inspects every rule to build a complete map of the concept hierarchy.

2.  **Handling Complexity:** This analysis handles complex rules with multiple conditions in the body. When it sees a rule like `a(X) :- b(X), c(X).`, it interprets `a` as a concept that is subsumed by the *common ancestors* of `b` and `c`. For example, if `b` is a `monster` and `c` is an `item`, and both are `element`s, the system will infer `subsumedBy(a, element)`. It currently does not infer that `a` is a sub-concept of `b` and `c` themselves, but rather of what they have in common up the hierarchy.

3.  **Storing the Results:** It then stores this complete map as simple `subsumedBy(child, parent).` facts in the database.

4.  **Querying:** By the time you run a query, the hard work is done. The reasoning rules are highly abstract and just consult the pre-computed facts. This makes the inheritance rules powerful and concise. For example, a single rule handles generalization for the `desirable` predicate:

    ```prolog
    % From prolog_meta/main.pl
    desirable(Perso, Obj):-
        desirable(SupPerso, SupObj),
        generalize_pair(Perso, Obj, SupPerso, SupObj).
    ```

**Analogy:** This is like reading a textbook, creating a detailed index, and then using that index to answer questions. The initial work is slow, but answering questions becomes very powerful.

### The `prolog_default` Strategy: Figure It Out On-the-Fly (Query-Time)

This approach's philosophy is: **do nothing until you're asked.**

1.  **No Analysis Phase:** It simply loads the Prolog files. Startup is extremely fast.

2.  **Query-Time Traversal:** When a query is made, its `subsumedBy/2` predicate starts a "live" search through the rules.

    ```prolog
    % From prolog_default/main.pl
    subsumedBy(Specific, General) :-
        nonvar(Specific),
        nonvar(General),
        GeneralPred =.. [General, _],
        clause(GeneralPred, BodyTerm),
        BodyTerm =.. [Intermediate|_],
        (Specific = Intermediate;  % Direct match
         subsumedBy(Specific, Intermediate)).  % Recursive check
    ```

3.  **The Critical Simplification:** Its traversal algorithm is intentionally simple and only supports rules with a single predicate in the body. For a rule like `a(X) :- b(X).`, the line `BodyTerm =.. [Intermediate|_]` correctly extracts `b` as the parent concept. However, for a rule with multiple goals, such as `a(X) :- b(X), c(X).`, the body is a term `','(b(X), c(X))`, whose main functor is the comma. The algorithm would try to find a parent named `,`, which is not a concept, and the subsumption check would fail. It is therefore blind to **both** `b` and `c`. This design forces a much simpler ontology, but is significantly faster as a result.

4.  **Less Abstract Rules:** Because the traversal is limited, the reasoning rules must be more explicit. It needs two separate rules for the `desirable` predicate to handle generalization over each argument:
    ```prolog
    % From prolog_default/main.pl
    desirable(Perso, Obj):-
        desirable(GenPerso, Obj), 
        subsumedBy(Perso, GenPerso).

    desirable(Perso, Obj):-
        desirable(Perso, GenObj), 
        subsumedBy(Obj, GenObj).
    ```

**Analogy:** This is like being given a textbook and a question. You use the table of contents to find a chapter and start reading, following references as you go. It's faster to start, but you might miss connections.

### Conclusion: Expressivity vs. Efficiency

The `prolog_default` approach has a simplified **reasoning algorithm**. It's not a simplified version of the codebase, but a different architecture. It trades the expressivity and completeness of the meta-analysis for speed.

This is a pragmatic and effective choice for the Wumpus World simulation, which is simple enough that `prolog_default` can handle its complexity while benefiting from the significant performance gains.
