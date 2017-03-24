# Init

Start by issuing
```
ant init
```

# Build

Build with
```
ant build
```

# Running

Run with

```
java -cp classes:jar/* ist.meic.pa.KeyConstructors <key-class>
```

(probably there's an easier way to run it through ant)

# Testing

```
java -cp .:classes:jar/* org.junit.runner.JUnitCore ist.meic.pa.FirstProjectTests
```

(probably there's an easier way to run it through ant)
