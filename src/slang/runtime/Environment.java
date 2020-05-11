package slang.runtime;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

public class Environment {
    private final Map<String, Value> environment = new HashMap<>();
    private final Environment parent;

    public Environment() {
        this(null);
    }

    public Environment(Environment parent) {
        this.parent = parent;
    }

    public void define(String name, Value value) {
        environment.put(name, value);
    }

    public Value get(String name) {
        Value val = getOrNull(name);
        if (val == null) {
            throw new RuntimeException("Unbound name '" + name + "'.");
        }
        return val;
    }

    public Environment with(String name, Value value) {
        environment.put(name, value);
        return this;
    }

    public Value getOrNull(String name) {
        if (!environment.containsKey(name)) {
            if (parent == null) {
                return null;
            } else {
                return parent.getOrNull(name);
            }
        }

        return environment.get(name);
    }

    public void set(String name, Value value) {
        if (environment.containsKey(name)) {
            environment.put(name, value);
        } else {
            if (parent == null) {
                throw new RuntimeException("Name '" + name + "' assigned to but has not been declared.");
            } else {
                parent.set(name, value);
            }
        }
    }

    public String shortString() {
        String repr = "[";
        boolean first = true;
        for (var entry : environment.entrySet()) {
            if (first) {
                first = false;
            } else {
                repr += ", ";
            }
            repr += entry.getKey() + "=";
            Object value = entry.getValue();
            if (value instanceof Matchbox || value instanceof MatchTree) {
                repr += "<Matchbox>";
            } else if (value instanceof Lazy) {
                repr += "{ ... }";
            } else {
                repr += value;
            }
        }
        repr += "]";
        return repr;
    }

    public String collapsedString() {
        Map<String, Object> collapsed = new HashMap<>();
        Stack<Environment> stack = new Stack<>();
        Environment current = this;

        while (current != null) {
            stack.push(current);
            current = current.parent;
        }

        while (!stack.isEmpty()) {
            current = stack.pop();
            collapsed.putAll(current.environment);
        }

        String repr = "[";
        boolean first = true;
        for (Map.Entry<String, Object> entry : collapsed.entrySet()) {
            if (first) {
                first = false;
            } else {
                repr += ", ";
            }
            repr += entry.getKey() + "=";
            Object value = entry.getValue();
            if (value instanceof Matchbox || value instanceof MatchTree) {
                repr += "<Matchbox>";
            } else if (value instanceof Lazy) {
                repr += "{ ... }";
            } else {
                repr += value;
            }
        }
        repr += "]";
        return repr;
    }

    public String toString() {
        return environment.toString();
    }

    public boolean isEmpty() {
        return environment.isEmpty();
    }

    public void update(Environment newBindings) {
        environment.putAll(newBindings.environment);
    }
}
