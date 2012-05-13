{-# LANGUAGE ForeignFunctionInterface #-}

#include <bindings.dsl.h>
#include <liblinear-1.91/linear.h>


module Data.Liblinear.Internal where
#strict_import

#starttype struct feature_node
#field index, CInt
#field value, CDouble
#stoptype

#starttype struct problem
#field l, CInt
#field n, CInt
#field y, Ptr CDouble
#field x, Ptr (Ptr <feature_node>)
#field bias, CDouble
#stoptype

#num L2R_LR
#num L2R_L2LOSS_SVC_DUAL
#num L2R_L2LOSS_SVC 
#num L2R_L1LOSS_SVC_DUAL
#num MCSVM_CS
#num L1R_L2LOSS_SVC 
#num L1R_LR
#num L2R_LR_DUAL
#num L2R_L2LOSS_SVR
#num L2R_L2LOSS_SVR_DUAL
#num L2R_L1LOSS_SVR_DUAL

#starttype struct parameter
#field solver_type, CInt
#field eps, CDouble
#field C, CDouble
#field nr_weight, CInt
#field weight_label, Ptr CInt
#field weight, Ptr CDouble
#field p, CDouble
#stoptype

#starttype struct model
#field param, <parameter>
#field nr_class, CInt
#field nr_feature, CInt
#field w, Ptr CDouble
#field label, Ptr CInt
#field bias, CDouble
#stoptype

#ccall train, Ptr <problem> -> Ptr <parameter> -> IO (Ptr <model>)
#ccall cross_validation, Ptr <problem> -> Ptr <parameter> -> CInt -> Ptr CDouble -> IO ()

#ccall predict_values, Ptr <model> -> Ptr <feature_node> -> Ptr CDouble -> IO CDouble
#ccall predict, Ptr <model> -> Ptr <feature_node> -> IO CDouble
#ccall predict_probability, Ptr <model> -> Ptr <feature_node> -> Ptr CDouble -> IO CDouble

#ccall save_model, Ptr CChar -> Ptr <model> -> IO CInt
#ccall load_model, Ptr CChar -> IO (Ptr <model>)

#ccall get_nr_feature, Ptr <model> -> IO CInt
#ccall get_nr_class, Ptr <model> -> IO CInt
#ccall get_labels, Ptr <model> -> Ptr CInt -> IO ()

#ccall free_model_content, Ptr <model> -> IO ()
#ccall free_and_destroy_model, Ptr (Ptr <model>) -> IO ()
#ccall destroy_param, Ptr <parameter> -> IO ()

#ccall check_parameter, Ptr <problem> -> Ptr <parameter> -> IO (Ptr CChar)
#ccall check_probability_model, Ptr <model> -> IO CInt
#ccall set_print_string_function, FunPtr (Ptr CChar -> IO ()) -> IO ()
