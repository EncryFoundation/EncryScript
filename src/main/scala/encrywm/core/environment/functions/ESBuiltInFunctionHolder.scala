package encrywm.core.environment.functions

import encrywm.backend.env.ESBuiltInFunc
import encrywm.core.environment.ESEnvComponent

trait ESBuiltInFunctionHolder extends ESEnvComponent { val asFunc: ESBuiltInFunc }
