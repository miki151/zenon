#include "lambda_capture_info.h"


const LambdaCaptureInfo::Var* LambdaCaptureInfo::find(const string& var) const {
  for (auto& elem : captures)
    if (elem.name == var)
      return &elem;
  return nullptr;
}
