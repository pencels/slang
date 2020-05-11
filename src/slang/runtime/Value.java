package slang.runtime;

import slang.parse.Pattern;

import java.util.*;
import java.util.stream.Collectors;

public abstract class Value {
    public abstract String getType();

    public Nothing asNothing() {
        throw new IllegalStateException("Cannot get Nothing from " + getType());
    }

    public double asDouble() {
        throw new IllegalStateException("Cannot get Double from " + getType());
    }

    // Different from toString(), returns the "nice" repr of Value.
    public abstract String asString();

    /* Subclass implementations! */

    public static class Nothing extends Value {
        private static final Nothing instance = new Nothing();
        private Nothing() {}
        public static Nothing getInstance() {
            return instance;
        }

        @Override
        public String getType() {
            return "Nothing";
        }

        @Override
        public String asString() {
            return toString();
        }

        @Override
        public String toString() {
            return "nothing";
        }
    }

    public static class Double extends Value {
        private double value;

        public Double(double value) {
            this.value = value;
        }

        @Override
        public String getType() {
            return "Double";
        }

        @Override
        public double asDouble() {
            return value;
        }

        @Override
        public String asString() {
            return toString();
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Double aDouble = (Double) o;
            return java.lang.Double.compare(aDouble.value, value) == 0;
        }

        @Override
        public int hashCode() {
            return Objects.hash(value);
        }

        @Override
        public String toString() {
            String text = String.valueOf(value);
            if (text.endsWith(".0")) {
                text = text.substring(0, text.length() - 2);
            }
            return text;
        }
    }

    public static class SlangString extends Value {
        private String value;

        public SlangString(String value) {
            this.value = value;
        }

        @Override
        public String getType() {
            return "String";
        }

        @Override
        public String asString() {
            return value;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            SlangString that = (SlangString) o;
            return Objects.equals(value, that.value);
        }

        @Override
        public int hashCode() {
            return Objects.hash(value);
        }

        @Override
        public String toString() {
            // TODO: handle escaping the string
            return "\"" + value + "\"";
        }
    }

    public static class Atom extends Value {
        private static final Map<String, Atom> atoms = new HashMap<>();
        private final String name;

        private Atom(String name) {
            this.name = name;
        }

        public static Atom create(String name) {
            if (!atoms.containsKey(name)) {
                synchronized(atoms) {
                    atoms.put(name, new Atom(name));
                }
            }
            return atoms.get(name);
        }

        @Override
        public String getType() {
            return "Atom";
        }

        @Override
        public String asString() {
            return toString();
        }

        @Override
        public String toString() {
            return ":" + name;
        }
    }

    public static class SlangList extends Value {
        private final List<Value> values;

        public SlangList(Collection<? extends Value> values) {
            this.values = List.copyOf(values);
        }

        public List<Value> getValues() {
            return values;
        }

        @Override
        public String getType() {
            return "List";
        }

        @Override
        public String asString() {
            return toString();
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            SlangList slangList = (SlangList) o;
            return Objects.equals(values, slangList.values);
        }

        @Override
        public int hashCode() {
            return Objects.hash(values);
        }

        @Override
        public String toString() {
            StringBuilder str = new StringBuilder("[");
            boolean first = true;
            for (Value value : values) {
                if (first) {
                    first = false;
                } else {
                    str.append(", ");
                }
                str.append(value);
            }
            str.append("]");
            return str.toString();
        }
    }
}
