#!/bin/sh

LRFILES="test-lr-basics-01.scm               \
         test-lr-basics-02.scm               \
         test-lr-basics-03.scm               \
         test-lr-basics-04.scm               \
         test-lr-basics-05.scm               \
         test-lr-error-recovery-01.scm       \
         test-lr-error-recovery-02.scm       \
         test-lr-no-clause.scm               \
         test-lr-associativity-01.scm        \
         test-lr-script-expression.scm       \
         test-lr-single-expressions.scm"


GRFILES="test-glr-basics-01.scm              \
         test-glr-basics-02.scm              \
         test-glr-basics-03.scm              \
         test-glr-basics-04.scm              \
         test-glr-basics-05.scm              \
         test-glr-associativity.scm          \
         test-glr-script-expression.scm      \
         test-glr-single-expressions.scm"

#echo "Testing v2.1.0"
#for item in $LRFILES; do
#    echo $item
#    gosh test.scm v2.1.0 $item
#done

echo "Testing v2.5.0"
for item in $LRFILES $GRFILES; do
    echo $item
    gosh test.scm v2.5.0 $item
done

echo "Testing v2.4.1"
for item in $LRFILES $GRFILES; do
    echo $item
    gosh test.scm v2.4.1 $item
done

### end of file
