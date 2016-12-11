# sqlscore 0.1.0
First release.

This version includes:

* Functions to generate CREATE TABLE and SELECT statements from model objects;
* Functions for generating unevaluated R expressions from model objects that correspond to
    * the model's linear predictor
    * the model's final prediction expression (the resposne function of the linear predictor)
* Support for built-in glm and lm objects, as well as
    * bayesglm from package:arm
    * cv.glmnet from package:glmnet
    * glmboost from package:mboost (only Gaussian models)
  Except for glmboost, all link functions that can be represented in SQL are supported for all packages.
* Using a custom link function by name. This is useful if, e.g., your database provides probit or tobit functions.
* Support for various formula features (in particular :, I() and model.matrix-style factor expansion).
