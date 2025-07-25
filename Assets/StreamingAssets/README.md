# Prolog Approaches for Wumpus World Agent

This document explains the different Prolog-based reasoning approaches implemented for the agents in this Wumpus World simulation. Each approach is contained in its own directory and represents a different trade-off between expressivity, performance, and complexity.

To run any of these examples, navigate to the specific directory and use SWI-Prolog:
```bash
cd <directory_name>
swipl main.pl
```

---

## Detailed Comparison: `prolog_default` vs. `prolog_meta`

The core difference between these two approaches is how they determine relationships between concepts (i.e., what is a "subclass" of what), which has significant implications for expressivity and performance.

### `prolog_default`: Direct and Explicit Hierarchy Traversal

This approach determines concept hierarchies by recursively stepping through Prolog rules at query time. The logic for this is explicitly defined in the `subsumedBy/2` predicate in [`prolog_default/main.pl`](./prolog_default/main.pl):

```prolog
subsumedBy(Specific, General) :-
    nonvar(Specific),
    nonvar(General),
    GeneralPred =.. [General, _],
    clause(GeneralPred, BodyTerm),
    BodyTerm =.. [Intermediate|_],
    (Specific = Intermediate;  % Direct match
     subsumedBy(Specific, Intermediate)).  % Recursive check
```

**What this means:**
- It checks for simple rules like `general(X) :- intermediate(X).`.
- It's a "hard-coded" way of traversing the hierarchy. It only looks for a single predicate in the body of a clause.
- **Limitation & Lower Expressivity:** It can only understand simple, direct inheritance. If you had a more complex rule like `conceptA(X) :- conceptB(X), conceptC(X).`, this `subsumedBy` predicate would only identify `conceptB` as the parent, ignoring `conceptC`.

The inheritance rules for properties also reflect this directness, requiring separate rules to generalize over each argument:
```prolog
% From prolog_default/main.pl
desirable(Perso, Obj):-
    desirable(GenPerso, Obj), 
    subsumedBy(Perso, GenPerso).

desirable(Perso, Obj):-
    desirable(Perso, GenObj), 
    subsumedBy(Obj, GenObj).
```

### `prolog_meta`: Abstract and Automated Hierarchy Construction

This approach uses **meta-programming** to build a complete, abstract understanding of the concept hierarchy *before* any queries are run. The key logic resides in [`prolog_meta/onto.pl`](./prolog_meta/onto.pl).

At startup, the `classification/2` predicate is called. It analyzes the **source code** of all the other `.pl` files, inspects the clauses, and automatically asserts a series of `subsumedBy(specific, general).` facts into the knowledge base.

**What this means:**
- **Higher Expressivity:** The system can understand more complex rules. For instance, the `subsume_intersection/2` predicate in `onto.pl` is designed to handle rules with multiple conditions in the body (e.g., `a(X) :- b(X), c(X).`). It correctly infers that both `b` and `c` are superclasses of `a`.
- **Abstraction:** You can define concepts with complex logical rules, and the meta-engine figures out the hierarchy for you. You are not limited to simple `is-a` chains.
- **More Powerful Inheritance:** The rules for inheriting properties are more generic and powerful. Instead of multiple direct rules, there is a single, more abstract rule using `generalize_pair/4`:

    ```prolog
    % From prolog_meta/main.pl
    desirable(Perso, Obj):-
        desirable(SupPerso, SupObj),
        generalize_pair(Perso, Obj, SupPerso, SupObj).
    ```
    This single rule can handle inheritance across both personality and objective simultaneously, which is more powerful and concise.

### Conclusion: Expressivity vs. Efficiency

- **`prolog_default` is less expressive than `prolog_meta`** It's limited to simple hierarchical rules. `prolog_meta`'s ability to analyze the code and handle more complex rule structures makes it fundamentally more expressive and flexible.

- **`prolog_meta` is less efficient than `prolog_default`** This power comes at a cost.
    1.  **Startup Time:** The initial `classification` step, where it reads and analyzes the entire knowledge base, is slower and more memory-intensive.
    2.  **Query Complexity:** The generic reasoning rules can lead to a larger search space for Prolog to explore when trying to find a solution, making individual queries slower.

The `prolog_default` approach is faster because it's more direct and avoids the heavy overhead of full meta-analysis. In the current Wumpus World simulation, which is fairly simple, the `prolog_default` approach is expressive enough to handle the complexity of the game.

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