/* Copyright 2020 Johannes Krude
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include QMK_KEYBOARD_H
#include "raw_hid.h"
#include "string.h"

enum layer_names {
  BASE
};

enum custom_keycodes {
  CS_LCTL,
  CS_LGUI,
  CS_LALT,

  CS_SWP_SWTCH
};

bool cs_swapped = false;
uint8_t cs_lctl_key = KC_LCTL;
uint8_t cs_lgui_key = KC_LGUI;
uint8_t cs_lalt_key = KC_LALT;

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
  case CS_LCTL:
    if (record->event.pressed)
      {
	register_code(cs_lctl_key);
      }
    else
      unregister_code(cs_lctl_key);
    break;

  case CS_LGUI:
    if (record->event.pressed)
      register_code(cs_lgui_key);
    else
      unregister_code(cs_lgui_key);
    break;

  case CS_LALT:
    if (record->event.pressed)
      register_code(cs_lalt_key);
    else
      unregister_code(cs_lalt_key);
    break;

  case CS_SWP_SWTCH:
    {
      raw_hid_send((uint8_t*)"swp", strlen("swp"));
    }
    if (record->event.pressed)
      {
	if (cs_swapped)
	  {
	    cs_lctl_key = KC_LCTL;
	    cs_lgui_key = KC_LGUI;
	    cs_lalt_key = KC_LALT;
	  }
	else
	  {
	    cs_lctl_key = KC_LGUI;
	    cs_lgui_key = KC_LALT;
	    cs_lalt_key = KC_LCTL;
	  }
	cs_swapped = !cs_swapped;
      }
    break;
  }

  return true;
}

void raw_hid_receive(uint8_t* data, uint8_t length) {
  raw_hid_send(data, length);
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

  /* Base           ,-----------------------------------------.     ,-----------------------------------------------------.
   *                | ESC | F1  | F2  | F3  | F4  | F5  | F6  |     | F7  | F8  | F9  | F10 | F11 | F12 |Print| Ins | Del |
   * ,-----------.  |-----+-----+-----+-----+-----+-----+-----|     |-----+-----+-----+-----+-----+-----+-----------+-----|
   * |  8  |  9  |  |  ~  |  1  |  2  |  3  |  4  |  5  |  6  |     |  7  |  8  |  9  |  0  |  -  |  =  | Backspac  | Home|
   * |-----+-----|  |-----------------------------------------'  ,--------------------------------------------------+-----|
   * |  6  |  7  |  | Tab   |  Q  |  W  |  E  |  R  |  T  |     |  Y  |  U  |  I  |  O  |  P  |  [  |  ]  |         | PgUp|
   * |-----+-----|  |---------------------------------------.    `------------------------------------------+ Enter +-----|
   * |  4  |  5  |  | Caps    |  A  |  S  |  D  |  F  |  G  |     |  H  |  J  |  K  |  L  |  ;  |  '  |  #  |       | PgDn|
   * |-----+-----|  |-----------------------------------------.   `-------------------------------------------------+-----|
   * |  2  |  3  |  | Shift | > |  Z  |  X  |  C  |  V  |  B  |     |  N  |  M  |  ,  |  .  |   /   |  Shift  | Up  | End |
   * |-----+-----|  |-----------------------------------------'   ,-------------------------------------------+-----+-----|
   * |  0  | SWP |  | Ctrl  |  GUI |  Alt |   Space   |Space|     | Space         |  Alt  | Slk | Ctrl  | Left| Down|Right|
   * `-----------'  `---------------------------------------'     `-------------------------------------------------------'
   */
  [BASE] = LAYOUT_iso(
		     //--------------------------------Left Hand-----------------------------------| |--------------------------------Right Hand------------------------------------------------
		                              KC_ESC,   KC_F1,   KC_F2,   KC_F3,  KC_F4,  KC_F5,  KC_F6,                KC_F7,  KC_F8,   KC_F9,  KC_F10,  KC_F11,   KC_F12,   KC_PSCR,  KC_INS,  KC_DEL,
		      KC_8,  KC_9,            KC_GRAVE, KC_1,    KC_2,    KC_3,   KC_4,   KC_5,   KC_6,                 KC_7,   KC_8,    KC_9,   KC_0,    KC_MINUS, KC_EQUAL, KC_BSPC,           KC_HOME,
		      KC_6,  KC_7,            KC_TAB,   KC_Q,    KC_W,    KC_E,   KC_R,   KC_T,                 KC_Y,   KC_U,   KC_I,    KC_O,   KC_P,    KC_LBRC,  KC_RBRC,                     KC_PGUP,
		      KC_4,  KC_5,            KC_CAPS,  KC_A,    KC_S,    KC_D,   KC_F,   KC_G,                 KC_H,   KC_J,   KC_K,    KC_L,   KC_SCLN, KC_QUOT,  KC_NUHS,  KC_ENTER,          KC_PGDN,
		      KC_2,  KC_3,            KC_LSFT,  KC_NUBS, KC_Z,    KC_X,   KC_C,   KC_V,   KC_B,                 KC_N,   KC_M,   KC_COMM, KC_DOT,  KC_SLSH,            KC_RSFT,  KC_UP,   KC_END,
		      KC_0,  CS_SWP_SWTCH,    CS_LCTL,  CS_LGUI, CS_LALT, KC_SPC, KC_SPC,                       KC_SPC,         KC_RALT, KC_SLCK ,KC_RCTL,                     KC_LEFT,  KC_DOWN, KC_RIGHT
		      )
};

/* Base           ,-----------------------------------------.     ,-----------------------------------------------------.
 *                |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |     |
 * ,-----------.  |-----+-----+-----+-----+-----+-----+-----|     |-----+-----+-----+-----+-----+-----+-----------+-----|
 * |     |     |  |     |     |     |     |     |     |     |     |     |     |     |     |     |     |           |     |
 * |-----+-----|  |-----------------------------------------'  ,--------------------------------------------------+-----|
 * |     |     |  |       |     |     |     |     |     |     |     |     |     |     |     |     |     |         |     |
 * |-----+-----|  |---------------------------------------.    `------------------------------------------+       +-----|
 * |     |     |  |         |     |     |     |     |     |     |     |     |     |     |     |     |     |       |     |
 * |-----+-----|  |-----------------------------------------.   `-------------------------------------------------+-----|
 * |     |     |  |       |   |     |     |     |     |     |     |     |     |     |     |       |         |     |     |
 * |-----+-----|  |-----------------------------------------'   ,-------------------------------------------+-----+-----|
 * |     |     |  |       |      |      |           |     |     |               |       |     |       |     |     |     |
 * `-----------'  `---------------------------------------'     `-------------------------------------------------------'
 */
// [] = LAYOUT(
//                    //--------------------------------Left Hand-----------------------------------------------| |--------------------------------Right Hand------------------------------------------------
//                    _______,  _______,  _______,  _______,  _______,  _______,  _______,              _______,   _______,  _______,  _______,  _______,  _______,  _______,  _______, _______,
//                    _______,  _______,    _______,  _______,  _______,  _______,  _______,  _______,  _______,              _______,   _______,  _______,  _______,  _______,  _______,  _______,           _______,
//                    _______,  _______,    _______,  _______,  _______,  _______,  _______,  _______,              _______,  _______,   _______,  _______,  _______,  _______,  _______,  _______,           _______,
//                    _______,  _______,    _______,  _______,  _______,  _______,  _______,  _______,              _______,  _______,   _______,  _______,  _______,  _______,  _______,                     _______,
//                    _______,  _______,    _______,  _______,  _______,  _______,  _______,  _______,              _______,  _______,   _______,  _______,  _______,                      _______, _______,  _______,
//                    _______,  _______,    _______,  _______,  _______,  _______,  _______,                        _______,             _______, _______,   _______,                      _______, _______,  _______
// ),
