package encrywm.builtins.environment.functions

import encrywm.backend.executor.context.ESBuiltInFunc
import encrywm.builtins.environment.ESEnvComponentHolder

trait ESBuiltInFunctionHolder extends ESEnvComponentHolder { val fn: ESBuiltInFunc }
