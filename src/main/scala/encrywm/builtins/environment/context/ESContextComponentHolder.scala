package encrywm.builtins.environment.context

import encrywm.builtins.Types.ESType
import encrywm.builtins.environment.ESEnvComponentHolder

trait ESContextComponentHolder extends ESEnvComponentHolder { val attrs: IndexedSeq[(String, ESType)] }
