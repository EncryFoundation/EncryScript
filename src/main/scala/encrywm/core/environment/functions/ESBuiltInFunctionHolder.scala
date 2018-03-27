package encrywm.core.environment.functions

import encrywm.backend.executor.context.ESBuiltInFunc
import encrywm.core.environment.ESEnvComponent

trait ESBuiltInFunctionHolder extends ESEnvComponent { val asFunc: ESBuiltInFunc }
