import com.hp.hpl.jena.rdf.model.ResourceFactory

/**
 * @author jiemakel
 *
 */

object FRBR {
  val ns = "http://iflastandards.info/ns/fr/frbr/frbroo/"
  def R(s: String) = ResourceFactory.createResource(ns+s)
  def P(s: String) = ResourceFactory.createProperty(ns+s)
  val Work = R("F1_Work")
  val Expression = R("F2_Expression")
  val Manifestation_Product_Type = R("F3_Manifestation_Product_Type")
  val Manifestation_Singleton = R("F4_Manifestation_Singleton")
  val Item = R("F5_Item")
  val Concept = R("F6_Concept")
  val Object = R("F7_Object")
  val Event = R("F8_Event")
  val Place = R("F9_Place")
  val Person = R("F10_Person")
  val Corporate_Body = R("F11_Corporate_Body")
  val Nomen = R("F12_Nomen")
  val Identifier = R("F13_Identifier")
  val Individual_Work = R("F14_Individual_Work")
  val Complex_Work = R("F15_Complex_Work")
  val Container_Work = R("F16_Container_Work")
  val Aggregation_Work = R("F17_Aggregation_Work")
  val Serial_Work = R("F18_Serial_Work")
  val Publication_Work = R("F19_Publication_Work")
  val Performance_Work = R("F20_Performance_Work")
  val Recording_Work = R("F21_Recording_Work")
  val Self_Contained_Expression = R("F22_Self-Contained_Expression")
  val Expression_Fragment = R("F23_Expression_Fragment")
  val Publication_Expression = R("F24_Publication_Expression")
  val Performance_Plan = R("F25_Performance_Plan")
  val Recording = R("F26_Recording")
  val Work_Conception = R("F27_Work_Conception")
  val Expression_Creation = R("F28_Expression_Creation")
  val Recording_Event = R("F29_Recording_Event")
  val Publication_Event = R("F30_Publication_Event")
  val Performance = R("F31_Performance")
  val Carrier_Production_Event = R("F32_Carrier_Production_Event")
  val Reproduction_Event = R("F33_Reproduction_Event")
  val KOS = R("F34_KOS")
  val Nomen_Use_Statement = R("F35_Nomen_Use_Statement")
  val Script_Conversion = R("F36_Script_Conversion")
  val Character = R("F38_Character")
  val Family = R("F39_Family")
  val Identifier_Assignment = R("F40_Identifier_Assignment")
  val Representative_Manifestation_Assignment = R("F41_Representative_Manifestation_Assignment")
  val Representative_Expression_Assignment = R("F42_Representative_Expression_Assignment")
  val Identifier_Rule = R("F43_Identifier_Rule")
  val Bibliographic_Agency = R("F44_Bibliographic_Agency")
  val Controlled_Access_Point = R("F50_Controlled_Access_Point")
  val Pursuit = R("F51_Pursuit")
  val Name_Use_Activity = R("F52_Name_Use_Activity")
  val Material_Copy = R("F53_Material_Copy")
  val Utilized_Information_Carrier = R("F54_Utilized_Information_Carrier")
  val should_have_type = P("CLP2_should_have_type")
  val should_be_type_of = P("CLP2i_should_be_type_of")
  val should_have_dimension = P("CLP43_should_have_dimension")
  val should_be_dimension_of = P("CLP43i_should_be_dimension_of")
  val should_consist_of = P("CLP45_should_consist_of")
  val should_be_incorporated_in = P("CLP45i_should_be_incorporated_in")
  val should_be_composed_of = P("CLP46_should_be_composed_of")
  val may_form_part_of = P("CLP46i_may_form_part_of")
  val should_have_number_of_parts = P("CLP57_should_have_number_of_parts")
  val subject_to = P("CLP104_subject_to")
  val applies_to = P("CLP104i_applies_to")
  val right_held_by = P("CLP105_right_held_by")
  val right_on = P("CLP105i_right_on")
  val should_carry = P("CLR6_should_carry")
  val should_be_carried_by = P("CLR6i_should_be_carried_by")
  val is_logical_successor_of = P("R1_is_logical_successor_of")
  val has_successor = P("R1i_has_successor")
  val is_derivative_of = P("R2_is_derivative_of")
  val has_derivative = P("R2i_has_derivative")
  val work_is_realised_in_expression = P("R3_is_realised_in")
  val expression_realises_work = P("R3i_realises")
  val carriers_provided_by = P("R4_carriers_provided_by")
  val comprises_carriers_of = P("R4i_comprises_carriers_of")
  val has_component = P("R5_has_component")
  val is_component_of = P("R5i_is_component_of")
  val carries = P("R6_carries")
  val is_carried_by = P("R6i_is_carried_by")
  val is_example_of = P("R7_is_example_of")
  val has_example = P("R7i_has_example")
  val consists_of = P("R8_consists_of")
  val forms_part_of = P("R8i_forms_part_of")
  val individual_work_is_realised_in_expression = P("R9_is_realised_in")
  val expression_realises_individual_work = P("R9i_realises")
  val has_member = P("R10_has_member")
  val is_member_of = P("R10i_is_member_of")
  val has_issuing_rule = P("R11_has_issuing_rule")
  val is_issuing_rule_of = P("R11i_is_issuing_rule_of")
  val performance_work_is_realised_in_performance_plan = P("R12_is_realised_in")
  val performance_plan_realises_performance_work = P("R12i_realises")
  val recording_work_is_realised_in_recording = P("R13_is_realised_in")
  val recordin_realises_recording_work = P("R13i_realises")
  val has_fragment = P("R15_has_fragment")
  val is_fragment_of = P("R15i_is_fragment_of")
  val initiated = P("R16_initiated")
  val was_initiated_by = P("R16i_was_initiated_by")
  val created_expression = P("R17_created")
  val expression_was_created_by = P("R17i_was_created_by")
  val created_manifestation = P("R18_created")
  val manifestation_was_created_by = P("R18i_was_created_by")
  val created_a_realization_of_work = P("R19_created_a_realization_of")
  val work_was_realised_through = P("R19i_was_realised_through")
  val recorded = P("R20_recorded")
  val was_recorded_through = P("R20i_was_recorded_through")
  val created_recording = P("R21_created")
  val recording_was_created_through = P("R21i_was_created_through")
  val created_a_realization_of_recording_work = P("R22_created_a_realization_of")
  val recording_work_was_realised_through = P("R22i_was_realised_through")
  val created_a_realization_of_publication_work = P("R23_created_a_realization_of")
  val publication_work_was_realised_through = P("R23i_was_realised_through")
  val created_publication_expression = P("R24_created")
  val publication_expression_was_created_through = P("R24i_was_created_through")
  val performed = P("R25_performed")
  val was_performed_in = P("R25i_was_performed_in")
  val produced_things_of_type = P("R26_produced_things_of_type")
  val thing_of_type_was_produced_by = P("R26i_was_produced_by")
  val used_as_source_material = P("R27_used_as_source_material")
  val was_used_by = P("R27i_was_used_by")
  val carrier_production_event_produced_item = P("R28_produced")
  val item_was_produced_by_carrier_production_event = P("R28i_was_produced_by")
  val reproduced = P("R29_reproduced")
  val was_reproduced_by = P("R29i_was_reproduced_by")
  val reproduction_event_produced_item = P("R30_produced")
  val item_was_produced_by_reproduction_event = P("R30i_was_produced_by")
  val is_reproduction_of = P("R31_is_reproduction_of")
  val has_reproduction = P("R31i_has_reproduction")
  val is_warranted_by = P("R32_is_warranted_by")
  val warrants = P("R32i_warrants")
  val has_content = P("R33_has_content")
  val has_validity_period = P("R34_has_validity_period")
  val is_validity_period_of = P("R34i_is_validity_period_of")
  val specified_by = P("R35_specified_by")
  val specifies = P("R35i_specifies")
  val uses_script_conversion = P("R36_uses_script_conversion")
  val is_script_conversion_used_in = P("R36i_is_script_conversion_used_in")
  val states_as_nomen = P("R37_states_as_nomen")
  val is_stated_as_nomen_in = P("R37i_is_stated_as_nomen_in")
  val refers_to_thema = P("R38_refers_to_thema")
  val is_thema_of = P("R38i_is_thema_of")
  val is_intended_for = P("R39_is_intended_for")
  val is_target_audience_in = P("R39i_is_target_audience_in")
  val has_representative_expression = P("R40_has_representative_expression")
  val is_representative_expression_for = P("R40i_is_representative_expression_for")
  val has_rep_manifestation_product_type = P("R41_has_rep_manifestation_product_type")
  val is_rep_manifestation_product_type_for = P("R41i_is_rep_manifestation_product_type_for")
  val is_representative_manifestation_singleton_for = P("R42_is_representative_manifestation_singleton_for")
  val has_representative_manifestation_singleton = P("R42i_has_representative_manifestation_singleton")
  val representative_manifestation_assignment_carried_out_by = P("R43_carried_out_by")
  val performed_representative_manifestation_assignment = P("R43i_performed")
  val representative_expression_assignment_carried_out_by = P("R44_carried_out_by")
  val performed_representative_expression_assignment = P("R44i_performed")
  val assigned_to_item = P("R45_assigned_to")
  val item_was_assigned_by = P("R45i_was_assigned_by")
  val assigned_to_identifier = P("R46_assigned_to")
  val identifier_was_assigned_by = P("R46i_was_assigned_by")
  val assigned_to_expression = P("R48_assigned_to")
  val expression_was_assigned_by = P("R48i_was_assigned_by")
  val assigned_to_product_type = P("R49_assigned")
  val product_type_was_assigned_by = P("R49i_was_assigned_by")
  val assigned_to_work = P("R50_assigned_to")
  val work_was_assigned_by = P("R50i_was_assigned_by")
  val assigned = P("R51_assigned")
  val was_assigned_by = P("R51i_was_assigned_by")
  val used_rule = P("R52_used_rule")
  val was_the_rule_used_in = P("R52i_was_the_rule_used_in")
  val assigned_to_manifestation = P("R53_assigned")
  val manifestation_was_assigned_by = P("R53i_was_assigned_by")
  val has_nomen_language = P("R54_has_nomen_language")
  val is_language_of_nomen_in = P("R54i_is_language_of_nomen_in")
  val has_nomen_form = P("R55_has_nomen_form")
  val is_nomen_form_in = P("R55i_is_nomen_form_in")
  val has_related_use = P("R56_has_related_use")
  val is_related_use_for = P("R56i_is_related_use_for")
  val is_based_on = P("R57_is_based_on")
  val is_basis_for = P("R57i_is_basis_for")
  val has_fictional_member = P("R58_has_fictional_member")
  val is_fictional_member_of = P("R58i_is_fictional_member_of")
  val had_typical_subject = P("R59_had_typical_subject")
  val was_typical_subject_of = P("R59i_was_typical_subject_of")
  val used_to_use_language = P("R60_used_to_use_language")
  val was_language_used_by = P("R60i_was_language_used_by")
  val occured_in_kind_of_context = P("R61_occured_in_kind_of_context")
  val was_kind_of_context_for = P("R61i_was_kind_of_context_for")
  val was_used_for_membership_in = P("R62_was_used_for_membership_in")
  val was_context_for = P("R62i_was_context_for")
  val named = P("R63_named")
  val was_named_by = P("R63i_was_named_by")
  val used_name = P("R64_used_name")
  val was_name_used_by = P("R64i_was_name_used_by")
  val recorded_aspects_of = P("R65_recorded_aspects_of")
  val had_aspects_recorded_through = P("R65i_had_aspects_recorded_through")
  val included_performed_version_of = P("R66_included_performed_version_of")
  val had_a_performed_version_through = P("R66i_had_a_performed_version_through")
}
