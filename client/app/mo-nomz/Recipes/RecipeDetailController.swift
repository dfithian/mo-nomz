//
//  RecipeDetailController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 7/5/21.
//

import UIKit

class RecipeDetailController: UIViewController {
    @IBOutlet weak var label: UILabel!
    @IBOutlet weak var star1: UIButton!
    @IBOutlet weak var star2: UIButton!
    @IBOutlet weak var star3: UIButton!
    @IBOutlet weak var star4: UIButton!
    @IBOutlet weak var star5: UIButton!
    @IBOutlet weak var link: UIButton!
    @IBOutlet weak var notes: UITextView!
    
    var recipe: RecipeWithId? = nil
    var onChange: (() -> Void)? = nil
    var beforeHeight: CGFloat? = nil
    var rating: Int? = nil
    
    @IBAction func didTapLink(_ sender: Any?) {
        if let link = recipe?.recipe.link {
            let url = URL(string: link)!
            if #available(iOS 10.0, *) {
                UIApplication.shared.open(url, options: [:], completionHandler: nil)
            } else {
                UIApplication.shared.openURL(url)
            }
        }
    }
    
    private func didTapStar(which: Int) {
        let filledStar = UIImage(systemName: "star.fill")
        let unfilledStar = UIImage(systemName: "star")
        rating = which
        switch which {
        case 0:
            self.star1.setImage(unfilledStar, for: .normal)
            self.star2.setImage(unfilledStar, for: .normal)
            self.star3.setImage(unfilledStar, for: .normal)
            self.star4.setImage(unfilledStar, for: .normal)
            self.star5.setImage(unfilledStar, for: .normal)
        case 1:
            self.star1.setImage(filledStar, for: .normal)
            self.star2.setImage(unfilledStar, for: .normal)
            self.star3.setImage(unfilledStar, for: .normal)
            self.star4.setImage(unfilledStar, for: .normal)
            self.star5.setImage(unfilledStar, for: .normal)
        case 2:
            self.star1.setImage(filledStar, for: .normal)
            self.star2.setImage(filledStar, for: .normal)
            self.star3.setImage(unfilledStar, for: .normal)
            self.star4.setImage(unfilledStar, for: .normal)
            self.star5.setImage(unfilledStar, for: .normal)
        case 3:
            self.star1.setImage(filledStar, for: .normal)
            self.star2.setImage(filledStar, for: .normal)
            self.star3.setImage(filledStar, for: .normal)
            self.star4.setImage(unfilledStar, for: .normal)
            self.star5.setImage(unfilledStar, for: .normal)
        case 4:
            self.star1.setImage(filledStar, for: .normal)
            self.star2.setImage(filledStar, for: .normal)
            self.star3.setImage(filledStar, for: .normal)
            self.star4.setImage(filledStar, for: .normal)
            self.star5.setImage(unfilledStar, for: .normal)
        default:
            self.star1.setImage(filledStar, for: .normal)
            self.star2.setImage(filledStar, for: .normal)
            self.star3.setImage(filledStar, for: .normal)
            self.star4.setImage(filledStar, for: .normal)
            self.star5.setImage(filledStar, for: .normal)
        }
        DispatchQueue.main.async {
            self.view.reloadInputViews()
        }
    }
    
    @IBAction func didTapStar1(_ sender: Any?) {
        didTapStar(which: 1)
    }
    
    @IBAction func didTapStar2(_ sender: Any?) {
        didTapStar(which: 2)
    }
    
    @IBAction func didTapStar3(_ sender: Any?) {
        didTapStar(which: 3)
    }
    
    @IBAction func didTapStar4(_ sender: Any?) {
        didTapStar(which: 4)
    }
    
    @IBAction func didTapStar5(_ sender: Any?) {
        didTapStar(which: 5)
    }
    
    @IBAction func didTapSave(_ sender: Any?) {
        if let r = recipe {
            let completion = {
                DispatchQueue.main.async {
                    self.dismiss(animated: true, completion: nil)
                }
                self.onChange?()
            }
            updateRecipe(id: r.id, active: r.recipe.active, rating: rating ?? r.recipe.rating, notes: notes.text, completion: completion)
        }
    }
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @objc func keyboardWillShow(notification: NSNotification) {
        beforeHeight = keyboardWillShowInternal(subview: notes, notification: notification)
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        keyboardWillHideInternal(heightMay: beforeHeight, notification: notification)
    }
    
    override func viewDidLoad() {
        self.label.text = recipe?.recipe.name
        self.notes.addDoneButtonOnKeyboard()
        self.notes.text = recipe?.recipe.notes ?? ""
        if recipe?.recipe.link == nil {
            self.link.removeFromSuperview()
        }
        self.didTapStar(which: recipe?.recipe.rating ?? 0)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
}
