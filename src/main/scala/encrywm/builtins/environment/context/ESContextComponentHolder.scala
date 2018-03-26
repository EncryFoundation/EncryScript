package encrywm.builtins.environment.context

import encrywm.builtins.Types.TYPE
import encrywm.builtins.environment.ESEnvComponentHolder

trait ESContextComponentHolder extends ESEnvComponentHolder { val attrs: IndexedSeq[(String, TYPE)] }
