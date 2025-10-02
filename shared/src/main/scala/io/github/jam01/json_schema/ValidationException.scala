/*
 * Copyright 2023 Jose Montoya
 * SPDX-License-Identifier: Apache-2.0
 */
package io.github.jam01.json_schema

/**
 * Exception thrown when validating with [[Config.ffast]].
 *
 * @param result the validation result
 */
class ValidationException(val result: OutputUnit) extends RuntimeException
