package ist.meic.pa;

import java.lang.reflect.Field;

import java.util.NoSuchElementException;
import java.util.stream.Stream;

public final class TemplateCode {

    private TemplateCode() { }

    /**
     * Asserts its arguments have even length.
     */
    public static void assertArgsHaveEvenLength(Object[] args) {
        if (args.length % 2 != 0) {
            throw new RuntimeException("Odd argument count.");
        }
    }

    /**
     * Searches for the first field of name fieldName on the hierarchy of
     * classes of obj (wether it is private, public, protected or
     * package-private) and sets its field to value.
     *
     * @param fieldName the name of the field to search for.
     * @param value the value to set the field to.
     * @param obj the obj for which we want to set the field.
     */
    public static void setField(String fieldName, Object value, Object obj) {
        try {
            final Field field = Utils.getHierarchy(obj.getClass())
                .stream()
                .flatMap(c -> Stream.of(c.getDeclaredFields()))
                .filter(f -> f.getName().equals(fieldName))
                .findFirst()
                .get();

            field.setAccessible(true);
            field.set(obj, value);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (NoSuchElementException e) {
            throw new RuntimeException("Unrecognized keyword: " + fieldName);
        }
    }

} // class TemplateCode
