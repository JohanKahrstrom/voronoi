This Scala version is based on the Java version by Zhenyu Pan,
which was based on a C++ version by Shane O'Sullivan, which in
turn was based on a C version by Stephen Fortune.

Work in progress.

I have discovered one bug, which can be seen by running the
tests: When calculating the Voronoi diagram for a set of
four points in a square, an extra empty line running from
(0.0, 0.0) to (0.0, -0.0) is constructed.