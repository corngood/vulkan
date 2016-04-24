module Spec.Pretty
  ( prettifySpec
  ) where

import           Spec.Command
import           Spec.Enum
import           Spec.Spec
import           Spec.Type
import           Write.Utils

prettifySpec :: Spec -> Spec
prettifySpec spec = spec { sTypes = nameType <$> sTypes spec
                         , sEnums = nameEnum <$> sEnums spec
                         , sCommands = commands
                         }
  where
    nameType (ADefine d) = ADefine d { dHsName = camelCase_ $ dHsName d }
    nameType (AStructType st) = AStructType st { stHsName = dropVK $ stHsName st
                                               , stMembers = nameStructMember <$> stMembers st
                                               }
    nameType (AUnionType ut) = AUnionType ut { utHsName = dropVK $ utHsName ut
                                             , utMembers = nameStructMember <$> utMembers ut
                                             }
    nameType (AHandleType h) = AHandleType h { htHsName = dropVK $ htHsName h }
    nameType (AnEnumType h) = AnEnumType h { etHsName = dropVK $ etHsName h }
    nameType (ABitmaskType h) = ABitmaskType h { bmtHsName = dropVK $ bmtHsName h }
    nameType t = t
    nameStructMember sm = sm { smHsName = recordName $ smHsName sm
                             }

    nameEnum e = e { eHsName = dropVK $ eHsName e }

    recordName "type" = "_type"
    recordName "module" = "_module"
    recordName "alignment" = "_alignment"
    recordName n = n

    commands = nameCommand <$> sCommands spec
    nameCommand c = c { cParameters = nameParameter <$> cParameters c
                      }
    nameParameter p = p


