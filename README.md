# sqlscore
R utilities to score GLMs and related models in SQL.

---

The sqlscore package provides utilities for generating sql queries
(particularly CREATE TABLE statements) from R model objects. The most important
use case is generating SQL to score a GLM or related model represented as an R
object, in which case the package handles parsing formula operators and
including the model's response function. The models scored need not be generalized
linear models, strictly speaking, but their prediction steps must consist of applying
a response function to a linear predictor. The package handles escaping and dealing
with formula operators, and provides a way to use a custom response function if desired.

### Function overview:
The SQL-generating functions create\_statement and select\_statement do what their
names suggest and generate CREATE TABLE and SELECT statements for model scoring.
Helper functions include linpred(), which generates an R call object representing
the linear predictor, and score_expression, an S3 generic that handles wrapping
the linear predictor in the response function.

### Supported models:
Specific packages and models that are known to work include: glm and lm from
package:stats, cv.glmnet from package:glmnet, glmboost from package:mboost,
and bayesglm from package:arm.

Default S3 methods are for objects structured like those of class "glm", so
models not listed here may work if they resemble those objects, but are not 
guaranteed to.
