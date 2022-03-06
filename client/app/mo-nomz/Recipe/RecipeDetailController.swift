//
//  RecipeDetailController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 7/5/21.
//

import UIKit

class RecipeDetailController: UIViewController, UITextViewDelegate, UITextFieldDelegate {
    @IBOutlet weak var nameRead: UILabel!
    @IBOutlet weak var nameWrite: UITextField!
    @IBOutlet weak var star1: UIButton!
    @IBOutlet weak var star2: UIButton!
    @IBOutlet weak var star3: UIButton!
    @IBOutlet weak var star4: UIButton!
    @IBOutlet weak var star5: UIButton!
    @IBOutlet weak var options: UIButton!
    
    var recipe: ReadableRecipeWithId? = nil
    var onChange: (() -> Void)? = nil
    var detailVc: RecipeDetailListController? = nil
    
    @IBAction func didTapLabel(_ sender: UITapGestureRecognizer) {
        nameWrite.becomeFirstResponder()
        nameWrite.alpha = 1
        nameRead.alpha = 0
    }
    
    func textFieldDidEndEditing(_ textField: UITextField) {
        guard let r = recipe, let name = nameWrite.text else { return }
        nameWrite.alpha = 0
        nameWrite.resignFirstResponder()
        nameRead.text = name
        nameRead.alpha = 1
        Database.updateRecipe(id: r.id, recipe: ReadableRecipe(name: name, link: r.recipe.link, active: r.recipe.active, rating: r.recipe.rating, notes: r.recipe.notes, ingredients: r.recipe.ingredients, steps: r.recipe.steps))
        onChange?()
    }
    
    private func didTapStar(which: Int) {
        let filledStar = UIImage(systemName: "star.fill")
        let unfilledStar = UIImage(systemName: "star")
        guard let r = recipe else { return }
        switch which {
        case 0:
            star1.setImage(unfilledStar, for: .normal)
            star2.setImage(unfilledStar, for: .normal)
            star3.setImage(unfilledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 1:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(unfilledStar, for: .normal)
            star3.setImage(unfilledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 2:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(unfilledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 3:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(filledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 4:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(filledStar, for: .normal)
            star4.setImage(filledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        default:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(filledStar, for: .normal)
            star4.setImage(filledStar, for: .normal)
            star5.setImage(filledStar, for: .normal)
        }
        if which != r.recipe.rating && which > 0 {
            let newRecipe = ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: r.recipe.active, rating: which, notes: r.recipe.notes, ingredients: r.recipe.ingredients, steps: r.recipe.steps)
            Database.updateRecipe(id: r.id, recipe: newRecipe)
            loadData()
            onChange?()
            DispatchQueue.main.async {
                self.view.reloadInputViews()
            }
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
    
    private func export(_ str: String) {
        let vc = UIActivityViewController(activityItems: [str], applicationActivities: nil)
        present(vc, animated: true, completion: nil)
    }
    
    private func setupOptions() {
        var actions = [UIAction]()
        if let link = recipe?.recipe.link, let encodedLink = link.addingPercentEncoding(withAllowedCharacters: .urlHostAllowed) {
            let linkUrl = Configuration.baseURL + "#/recipe?recipe_url=" + encodedLink
            actions.append(UIAction(title: "Open in Browser", image: UIImage(systemName: "safari"), handler: { _ in Browser.visitLink(link) }))
            actions.append(UIAction(title: "Share Recipe", image: UIImage(systemName: "square.and.arrow.up"), handler: { _ in self.export(linkUrl) }))
        }
        if let r = recipe?.recipe {
            let ingredientText = Configuration.nomzPrefix + "\n" + "Ingredients for " + r.name + ":" + "\n" + r.ingredients.map({ $0.value.render() }).joined(separator: "\n")
            actions.append(UIAction(title: "Share Ingredients", image: UIImage(systemName: "list.dash"), handler: { _ in self.export(ingredientText) }))
        }
        if actions.isEmpty {
            options.removeFromSuperview()
        } else {
            options.menu = UIMenu(options: .displayInline, children: actions)
            options.showsMenuAsPrimaryAction = true
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        guard let r = recipe else { return }
        if let vc = segue.destination as? RecipeDetailListController, segue.identifier == "embedDetail" {
            detailVc = vc
            loadData()
            vc.recipe = r
            vc.steps = r.recipe.steps.map({ StepWithId(id: $0, step: $1) }).sorted(by: { $0.step.order < $1.step.order })
            vc.ingredients = r.recipe.ingredients.map({ ReadableIngredientWithId(id: $0, ingredient: $1) }).sorted(by: { $0.ingredient.order < $1.ingredient.order })
            vc.onChange = { () -> Void in
                self.onChange?()
                self.loadData()
            }
        }
    }
    
    private func loadData() {
        guard let r = recipe else { return }
        guard let newRecipe = Database.getRecipe(id: r.id) else { return }
        recipe = newRecipe
        detailVc?.recipe = newRecipe
        detailVc?.steps = newRecipe.recipe.steps.map({ StepWithId(id: $0, step: $1) }).sorted(by: { ($0.step.order, $0.step.step) < ($1.step.order, $1.step.step) })
        detailVc?.ingredients = newRecipe.recipe.ingredients.map({ ReadableIngredientWithId(id: $0, ingredient: $1) }).sorted(by: { ($0.ingredient.order, $0.ingredient.name) < ($1.ingredient.order, $1.ingredient.name) })
        DispatchQueue.main.async {
            self.detailVc?.tableView.reloadData()
        }
    }
    
    override func viewDidLoad() {
        nameRead.text = recipe?.recipe.name
        nameWrite.text = recipe?.recipe.name
        didTapStar(which: recipe?.recipe.rating ?? 0)
        nameWrite.addDoneButtonOnKeyboard()
        setupOptions()
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadData()
    }
}
