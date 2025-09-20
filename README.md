# Telemedicine-Appointment-System

Secure telehealth appointments with e-prescriptions and insurance billing (concept scope). This repository includes two independent Clarity contracts, built without traits or cross-contract calls:

- appointment-scheduler: Time-slot booking and provider availability
- e-prescription-service: Prescription issuance and fill tracking

Overview

- Providers register and manage availability slots.
- Patients book appointments in open slots (capacity-aware).
- Prescribers issue e-prescriptions; pharmacies fill them according to refills.

Principles

- Minimal state, explicit authorization, and deterministic logic.
- Clean Clarity v3 syntax; no reliance on external time sources.
- Separate contracts by concern; no interdependence.

Contracts

1) appointment-scheduler
- Counters for providers, slots, and appointments
- Providers: owner principal, name, specialty, active flag
- Slots: day, start/end minute, capacity, booked count, active flag
- Appointments: patient, slot, provider, canceled flag, notes
- Actions: register-provider, open-slot, book, cancel-appointment, close-slot, reopen-slot
- Read-only: get-provider, get-slot, get-appointment, slot-availability

2) e-prescription-service
- Counter for prescriptions (rx-id)
- Routes (prescriptions): prescriber, patient, drug, dosage, quantity, refills, issued flag, canceled, filled-count
- Fill records by index per rx
- Actions: issue, cancel, fill, predict next-eta (not applicable here; only fill/issue)
- Read-only: get-rx, get-fill, remaining-fills

Local development

- clarinet check
  Compile and type-check contracts.

Branches

- main: Initialization and README only
- development: Contracts and tests

License

MIT
